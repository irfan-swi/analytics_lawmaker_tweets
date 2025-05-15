# app_async.R - Asynchronous loading version
library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(gdtools)
library(data.table)
library(promises)
library(future)

# Enable async processing
plan(multisession)

# Register the font
register_gfont("Roboto")

# Load pre-calculated data immediately
precalc <- readRDS("precalculated_data.rds")
default_plot_data <- precalc$default_plot_data
unique_months <- precalc$unique_months
issue_choices <- precalc$issue_choices
default_start_date <- precalc$default_dates$start
default_end_date <- precalc$default_dates$end

# Load full data asynchronously
data_promise <- future({
  readRDS("preprocessed_tweets.rds")
}) %...>% {
  message("Full data loaded")
  .
}

# Define UI
ui <- fluidPage(
  includeCSS("styles.css"),
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/d3@5/dist/d3.min.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # Show loading indicator
  conditionalPanel(
    condition = "!output.data_loaded",
    tags$div(
      class = "loading-indicator",
      tags$p("Loading full dataset..."),
      tags$div(class = "spinner")
    )
  ),
  
  tags$div(id = "custom-html-section"),
  
  fluidRow(
    column(
      width = 12,
      addGFontHtmlDependency(family = c("Roboto")),
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", issue_choices),
                      selected = "All"),
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("Both Chambers", "House", "Senate"),
                      selected = "Both Chambers"),
          selectInput("selected_party",
                      "Party",
                      choices = c("All Parties", "Democrat", "Republican", "Independent"),
                      selected = "All Parties"),
          selectInput("num_lawmakers",
                      "Number of lawmakers",
                      choices = c("10", "20", "50"),
                      selected = "20"),
          selectInput("start_date",
                      "Start Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = default_start_date),
          selectInput("end_date",
                      "End Date",
                      choices = setNames(unique_months$month_date, unique_months$date_label),
                      selected = default_end_date)
        )
      )
    ),
    
    column(
      width = 12,
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$script("Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });")
    )
  )
)

# Server with async data loading
server <- function(input, output, session) {
  # Track data loading state
  values <- reactiveValues(
    full_data = NULL,
    data_loaded = FALSE,
    last_plot_data = default_plot_data
  )
  
  # Load full data when ready
  data_promise %...>% {
    isolate({
      values$full_data <- .
      values$data_loaded <- TRUE
    })
  }
  
  # Output for conditional panel
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Check if using default view
  use_default_data <- reactive({
    input$selected_issue == "All" &&
      input$selected_chamber == "Both Chambers" &&
      input$selected_party == "All Parties" &&
      input$num_lawmakers == "20" &&
      input$start_date == as.character(default_start_date) &&
      input$end_date == as.character(default_end_date)
  })
  
  # Filter data
  filtered_data <- reactive({
    # Use pre-calculated default if possible
    if (use_default_data()) {
      return(default_plot_data)
    }
    
    # Wait for full data to load
    req(values$data_loaded)
    df <- values$full_data
    
    # Apply filters
    start_date <- as.Date(input$start_date)
    end_date <- as.Date(input$end_date)
    
    filtered_df <- df[date >= start_date & date <= end_date]
    
    if (input$selected_chamber != "Both Chambers") {
      filtered_df <- filtered_df[chamber == input$selected_chamber]
    }
    
    if (input$selected_party != "All Parties") {
      filtered_df <- filtered_df[party_clean == input$selected_party]
    }
    
    if (input$selected_issue != "All") {
      filtered_df <- filtered_df[issue_name == input$selected_issue]
    }
    
    if(nrow(filtered_df) == 0) {
      return(NULL)
    }
    
    # Aggregate
    plot_df <- filtered_df[, .(posts = uniqueN(comm_content_id)), 
                           by = .(display_name, party_name, party_clean, person_id)]
    
    if (input$num_lawmakers != "All") {
      num_to_show <- as.integer(input$num_lawmakers)
      plot_df <- plot_df[order(-posts)][seq_len(min(num_to_show, .N))]
    }
    
    values$last_plot_data <- plot_df
    plot_df
  })
  
  # Rest of server code remains the same...
  output$plot <- renderGirafe({
    plot_data <- filtered_data()
    
    if (is.null(plot_data)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for the selected filters", 
                 size = 5, hjust = 0.5) +
        theme_void()
      return(girafe(ggobj = p))
    }
    
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
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
            axis.text = element_text(color="black", size = 10),
            legend.title = element_text(color="black", size = 8),
            legend.text = element_text(color="black", size = 7))
    
    girafe(ggobj = p, 
           width_svg = 10, 
           height_svg = max(0.3 * nrow(plot_data), 5),
           options = list(
             opts_hover(css = "cursor:pointer;fill:gray;stroke:gray;"),
             opts_selection(type = "single", css = "fill:gray;stroke:gray;"),
             opts_toolbar(saveaspng = FALSE)
           ))
  })
  
  observeEvent(input$plot_selected, {
    selected_id <- input$plot_selected
    if (!is.null(selected_id) && length(selected_id) > 0) {
      url <- paste0("https://app.legis1.com/lawmaker/detail?id=", selected_id, "#communications")
      session$sendCustomMessage("openURL", url)
    }
  })
  
  output$main_title <- renderText({
    parts <- character()
    if(input$selected_chamber != "Both Chambers") parts <- c(parts, input$selected_chamber)
    if(input$selected_party != "All Parties") parts <- c(parts, input$selected_party)
    
    prefix <- if(length(parts) > 0) paste(parts, collapse = " ") else ""
    issue <- if(input$selected_issue == "All") "All Issues" else input$selected_issue
    
    paste0(prefix, if(nchar(prefix) > 0) " ", "Lawmaker Twitter Activity: ", issue)
  })
  
  output$subtitle <- renderText({
    start_label <- unique_months[month_date == as.Date(input$start_date), date_label]
    end_label <- unique_months[month_date == as.Date(input$end_date), date_label]
    paste0(start_label, " - ", end_label)
  })
}

shinyApp(ui = ui, server = server)
