library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(gdtools)
library(data.table)

# Register the font
register_gfont("Roboto")

# Global variables for default values
DEFAULT_CHAMBER <- "Both Chambers"
DEFAULT_PARTY <- "All Parties"
DEFAULT_ISSUE <- "All"
DEFAULT_NUM_LAWMAKERS <- "20"

# Pre-process data once at startup
startup_time <- Sys.time()
message("Starting data load...")

# Use fread with optimizations
df <- fread("https://legis1-analytics.s3.us-east-1.amazonaws.com/1_tweets_df.csv",
            showProgress = FALSE,
            data.table = TRUE,
            nThread = parallel::detectCores())

# Convert to data.table operations for speed
df[, date := as.Date(date, format = "%Y-%m-%d")]
df[, date_label := paste(month, year)]
df[, party_clean := gsub(",.*", "", party_name)]

# Pre-calculate default view data
unique_months <- df[, .(month_date = as.Date(paste0(year, "-", match(month, month.name), "-01")),
                        date_label = date_label[1]),
                    by = .(year, month)][order(-month_date)]

# Get default date range (most recent two months)
default_end_date <- unique_months$month_date[1]
default_start_date <- unique_months$month_date[2]

# Pre-filter for default view
default_filtered <- df[date >= default_start_date & date <= default_end_date]

# Pre-aggregate default data
default_plot_data <- default_filtered[, .(posts = uniqueN(comm_content_id)), 
                                      by = .(display_name, party_name, party_clean, person_id)
][order(-posts)][1:as.numeric(DEFAULT_NUM_LAWMAKERS)]

message(paste("Data loaded in", round(Sys.time() - startup_time, 2), "seconds"))

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
                      selected = DEFAULT_ISSUE),
          # Select Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("Both Chambers", "House", "Senate"),
                      selected = DEFAULT_CHAMBER),
          # Select Party
          selectInput("selected_party",
                      "Party",
                      choices = c("All Parties", "Democrat", "Republican", "Independent"),
                      selected = DEFAULT_PARTY),
          # Select Number of Lawmakers to display
          selectInput("num_lawmakers",
                      "Number of lawmakers",
                      choices = c("10", "20", "50"),
                      selected = DEFAULT_NUM_LAWMAKERS),
          selectInput("start_date",
                      "Start Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = default_start_date
          ),
          selectInput("end_date",
                      "End Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = default_end_date
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
  
  # Reactive values to cache computations
  values <- reactiveValues(
    last_plot_data = default_plot_data
  )
  
  # Check if we can use cached data
  use_default_data <- reactive({
    input$selected_issue == DEFAULT_ISSUE &&
      input$selected_chamber == DEFAULT_CHAMBER &&
      input$selected_party == DEFAULT_PARTY &&
      input$num_lawmakers == DEFAULT_NUM_LAWMAKERS &&
      input$start_date == as.character(default_start_date) &&
      input$end_date == as.character(default_end_date)
  })
  
  # Process data with caching
  filtered_data <- reactive({
    # Use pre-computed default data if possible
    if (use_default_data()) {
      return(default_plot_data)
    }
    
    # Otherwise, compute on demand using data.table for speed
    filtered_df <- df[date >= as.Date(input$start_date) & date <= as.Date(input$end_date)]
    
    # Filter by chamber
    if (input$selected_chamber != "Both Chambers") {
      filtered_df <- filtered_df[chamber == input$selected_chamber]
    }
    
    # Filter by party
    if (input$selected_party != "All Parties") {
      filtered_df <- filtered_df[party_clean == input$selected_party]
    }
    
    # Filter by selected issue
    if (input$selected_issue != "All") {
      filtered_df <- filtered_df[issue_name == input$selected_issue]
    }
    
    # Check if the filtered dataframe is empty
    if(nrow(filtered_df) == 0) {
      return(NULL)
    }
    
    # Prepare data for plotting using data.table operations
    if (input$selected_issue == "All") {
      # Get unique posts per lawmaker
      plot_df <- filtered_df[, .(posts = uniqueN(comm_content_id)), 
                             by = .(display_name, party_name, party_clean, person_id)]
    } else {
      plot_df <- filtered_df[, .(posts = uniqueN(comm_content_id)), 
                             by = .(display_name, party_name, party_clean, person_id)]
    }
    
    # Select top lawmakers based on user input
    if (input$num_lawmakers != "All") {
      plot_df <- plot_df[order(-posts)][1:as.numeric(input$num_lawmakers)]
    }
    
    values$last_plot_data <- plot_df
    plot_df
  })
  
  output$plot <- renderGirafe({
    plot_data <- filtered_data()
    
    if (is.null(plot_data)) {
      # Return an empty plot with message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for the selected filters", 
                 size = 5, hjust = 0.5) +
        theme_void()
      return(girafe(ggobj = p))
    }
    
    # Set colors
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
    # Plot
    p <- ggplot(plot_data, aes(x = reorder(display_name, posts), y = posts, fill = party_clean)) +
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
      url <- paste0("https://app.legis1.com/lawmaker/detail?id=", selected_id, "#communications")
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
    start_label <- unique_months[month_date == as.Date(input$start_date), date_label]
    end_label <- unique_months[month_date == as.Date(input$end_date), date_label]
    paste0(start_label, " - ", end_label)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
