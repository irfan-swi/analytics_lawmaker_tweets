library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(gdtools)
library(data.table)

# Register the font
register_gfont("Roboto")

# Load the dataset from S3 bucket
df <- fread("https://legis1-analytics.s3.us-east-1.amazonaws.com/df2.csv")

# Convert date column to Date type for proper sorting
df$date <- as.Date(df$date, format = "%Y-%m-%d")

# Create a display label combining month and year
df$date_label <- paste(df$month, df$year)

# Get unique month-year combinations for dropdown
unique_months <- df %>%
  mutate(
    # Create a standardized date for each month (1st of the month)
    month_date = as.Date(paste0(year, "-", match(month, month.name), "-01"))
  ) %>%
  select(month_date, date_label) %>%
  distinct() %>%
  arrange(desc(month_date))

# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section"
  ),
  
  fluidRow(
    # Selection Panel
    column(
      width = 12,
      addGFontHtmlDependency(family = c("Roboto")),
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          # Select Issue
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", sort(unique(df$issue_name))),
                      selected = "All"),
          # Select Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("Both Chambers", "House", "Senate"),
                      selected = "Both Chambers"),
          # Select Party
          selectInput("selected_party",
                      "Party",
                      choices = c("All Parties", "Democrat", "Republican", "Independent"),
                      selected = "All Parties"),
          # Select Number of Lawmakers to display
          selectInput("num_lawmakers",
                      "Number of lawmakers",
                      choices = c("10", "20", "50"),
                      selected = "20"),
          selectInput("start_date",
                      "Start Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = unique_months$month_date[2]  # Second most recent
          ),
          selectInput("end_date",
                      "End Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = unique_months$month_date[1]  # Most recent
          )
        ),
      ),
    ),
    
    # Main Panel
    column(
      width = 12,
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$plot <- renderGirafe({
    # Filter by date range
    filtered_df <- df %>%
      filter(date >= as.Date(input$start_date) & date <= as.Date(input$end_date))
    
    # Filter by chamber
    if (input$selected_chamber != "Both Chambers") {
      filtered_df <- filtered_df %>% filter(chamber == input$selected_chamber)
    }
    
    # Filter by party
    if (input$selected_party != "All Parties") {
      # Clean party names in the data to match the dropdown values
      filtered_df <- filtered_df %>% 
        mutate(party_clean = gsub(",.*", "", party_name)) %>%
        filter(party_clean == input$selected_party)
    }
    
    # Filter by selected issue
    if (input$selected_issue != "All") {
      filtered_df <- filtered_df %>% filter(issue_name == input$selected_issue)
    }
    
    # Check if the filtered dataframe is empty
    if(nrow(filtered_df) == 0) {
      # Return an empty plot with message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for the selected filters", 
                 size = 5, hjust = 0.5) +
        theme_void()
      return(girafe(ggobj = p))
    }
    
    # Prepare data for plotting - count unique comm_content_ids
    plot_df <- filtered_df %>%
      group_by(display_name, party_name, person_id) %>%
      summarise(posts = n_distinct(comm_content_id), .groups = "drop")
    
    # Select top lawmakers based on user input
    if (input$num_lawmakers != "All") {
      top_lawmakers <- plot_df %>%
        arrange(desc(posts)) %>%
        head(as.numeric(input$num_lawmakers))
      plot_df <- plot_df %>% filter(display_name %in% top_lawmakers$display_name)
    }
    
    # Set colors
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
    # Clean party names for color mapping
    plot_df <- plot_df %>%
      mutate(party_clean = gsub(",.*", "", party_name))
    
    # Plot
    p <- ggplot(plot_df, aes(x = reorder(display_name, posts), y = posts, fill = party_clean)) +
      geom_bar_interactive(stat = "identity", 
                           aes(tooltip = paste0(display_name, ': ', posts, ' posts'), 
                               data_id = as.numeric(person_id))) +
      labs(x = "Lawmaker", y = "Number of Twitter Posts", fill = "Party") +
      coord_flip() +
      scale_fill_manual(values = party_colors, 
                        limits = c("Democrat", "Republican", "Independent")) +
      theme_classic(base_family = "Roboto") +
      theme(axis.title = element_text(color="black", size = 8),
            axis.text = element_text(color="black", size = 6),
            legend.title = element_text(color="black", size = 8),
            legend.text = element_text(color="black", size = 7))
    
    girafe(ggobj = p, options = list(
      opts_hover(css = "cursor:pointer;fill:gray;stroke:gray;"),
      opts_selection(type = "single", css = "fill:gray;stroke:gray;")
    ))
  })
  
  # Handle bar clicks
  observeEvent(input$plot_selected, {
    selected_id <- input$plot_selected
    if (!is.null(selected_id) && length(selected_id) > 0) {
      url <- paste0("https://congress-qa.sunwater.org/lawmaker/detail?id=", selected_id, "#communications")
      session$sendCustomMessage("openURL", url)
    }
  })
  
  # Dynamic title and subtitle
  output$main_title <- renderText({
    chamber_text <- if(input$selected_chamber == "Both Chambers") "" else paste0(input$selected_chamber, " ")
    party_text <- if(input$selected_party == "All Parties") "" else paste0(input$selected_party, " ")
    issue_text <- if(input$selected_issue == "All") "All Issues" else input$selected_issue
    
    paste0(chamber_text, party_text, "Lawmaker Twitter Activity: ", issue_text)
  })
  
  output$subtitle <- renderText({
    # Get the labels for display
    start_label <- unique_months %>% filter(month_date == as.Date(input$start_date)) %>% pull(date_label)
    end_label <- unique_months %>% filter(month_date == as.Date(input$end_date)) %>% pull(date_label)
    paste0(start_label, " - ", end_label)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
