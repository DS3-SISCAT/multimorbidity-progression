library(shiny)
library(markdown)

# Base directory
base_dir <- "./" 

# Create a public path to access the files
shiny::addResourcePath("networks", base_dir)

# List of available folders (chronic diseases)
get_folders <- function() {
  list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
}

# Find HTML files in a subfolder
get_html_files <- function(folder) {
  full_path <- file.path(base_dir, folder)
  html_files <- list.files(full_path, pattern = "\\.html$", full.names = TRUE)
  return(html_files)
}

# User Interface
ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript", src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  titlePanel("Multimorbidity disease trajectory networks"),
  tabsetPanel(
    tabPanel(
      "Network Explorer",
      fluidPage(
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 20px; margin-bottom: 20px;",
              selectInput(
                inputId = "selected_folder",
                label = "Select the network by chronic disease:",
                choices = get_folders(),
                selected = NULL,
                width = "100%"
              )
            ),
            div(
              style = "margin-top: 10px; margin-bottom: 20px;",
              p("Instructions for Network Exploration:"),
              p(
                "- ", strong("Nodes:"), 
                " Click on a node to view information about the disease it represents, including its prevalence N (%) in the subpopulation of the network, which consists of all patients with the selected chronic disease. It also includes the strata ratio which indicates the proportion of cases that the disease appears before the high risk strata of AMG index."
              ),
              p(
                "- ", strong("Edges:"), 
                " Click on an edge (connection) between two nodes to obtain detailed information about the association type (Pre-Transition, Post-Transition, or Mixed-Transition), the number of connections, and the Adjusted Risk Ratio between the two diseases."
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            uiOutput("network_view")
          )
        )
      )
    ),
    tabPanel(
      "Methods",
      fluidPage(
        includeMarkdown(paste0(base_dir, "methods.md"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Render the selected network HTML file in an iframe
  output$network_view <- renderUI({
    req(input$selected_folder)
    html_files <- get_html_files(input$selected_folder)
    if (length(html_files) > 0) {
      public_path <- file.path(
        "networks",
        input$selected_folder,
        basename(html_files[1]) # Automatically selects the first HTML file
      )
      tags$iframe(
        src = public_path,
        style = "width: 100%; height: 600px; border: none;"
      )
    } else {
      HTML("<p>No HTML file found in this folder.</p>")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
