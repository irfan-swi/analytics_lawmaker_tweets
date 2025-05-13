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

# Format dates to show full month and year
df$date <- as.Date(df$pub_date, format = "%Y-%m-%d")
df$date_display <- format(df$date, "%B %Y")  # Full month name and year
df$date_sort <- as.Date(paste0(format(df$date, "%Y-%m"), "-01"))

# Aggregate data by month and other variables
df <- df %>%
  group_by(date_sort, date_display, member_id, person_id, chamber, issue_name, swi_issue_id, 
           display_name, formal_title, first_name, last_name, party_name, us_state_id, district_no) %>%
  summarise(posts = n(), .groups = "drop") %>%
  ungroup() %>%
  arrange(date_sort)

# Clean up issue names (remove trailing asterisks)
df$issue_name <- gsub("\\*\\*$", "", df$issue_name)

# Create chart name column
df <- df %>%
  mutate(chart_name = paste0(substr(display_name, 1, 3), ". ", last_name, " ", 
                            sub(".*\\s(\\S+)$", "\\1", display_name)))

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
                      choices = rev(unique(df$date_display)),
                      selected = tail(unique(df$date_display), 2)[1]
          ),
          selectInput("end_date",
                      "End Date",
                      choices = rev(unique(df$date_display)),
                      selected = tail(unique(df$date_display), 1)
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
    # Filter by date
    start_date <- df %>% filter(date_display == input$start_date) %>% pull(date_sort)
    end_date <- df %>% filter(date_display == input$end_date) %>% pull(date_sort)
    
    filtered_df <- subset(df, date_sort >= unique(start_date) & date_sort <= unique(end_date))
    
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
    
    # Prepare data for plotting
    plot_df <- filtered_df %>%
      group_by(display_name, party_name, chart_name, person_id) %>%
      summarise(posts = sum(posts), .groups = "drop")
    
    # Select top lawmakers based on user input
    if (input$num_lawmakers != "All") {
      top_lawmakers <- plot_df %>%
        arrange(desc(posts)) %>%
        head(as.numeric(input$num_lawmakers))
      plot_df <- plot_df %>% filter(chart_name %in% top_lawmakers$chart_name)
    }
    
    # Set colors
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
    # Clean party names for color mapping
    plot_df <- plot_df %>%
      mutate(party_clean = gsub(",.*", "", party_name))
    
    # Plot
    p <- ggplot(plot_df, aes(x = reorder(chart_name, posts), y = posts, fill = party_clean)) +
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
    paste0(input$start_date, " - ", input$end_date)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
