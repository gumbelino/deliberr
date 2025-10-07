#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(deliberr)
library(DT)

# Assume 'surveys' and 'roles' are loaded, typically from deliberr or a data file.
# We'll use deliberr data here directly for choices initialization.
if (!exists("surveys")) {
  surveys <- data.frame(name = unique(deliberr::human_data$survey))
}
if (!exists("roles")) {
  roles <- data.frame(uid = unique(deliberr::human_data$role_uid))
}

# --- User Interface (UI) Definition ---
fluidPage(
  # App title
  titlePanel("deliberr"),

  # Main area now uses a tabsetPanel for different views
  tabsetPanel(
    # Tab 1: Case Analysis (Existing content)
    tabPanel("Case Analysis", sidebarLayout(
      sidebarPanel(
        h4("Analysis Options"),
        # Input for selecting the statistical method
        selectInput(
          "method",
          "Method:",
          choices = c("wilcox", "t.test"),
          selected = "wilcox"
        ),
        # Input for selecting the alternative hypothesis
        selectInput(
          "alternative",
          "Alternative Hypothesis:",
          choices = c("greater", "two.sided", "less"),
          selected = "greater"
        ),
        # Checkbox for using adjusted p-values
        checkboxInput("adjusted", "Use adjusted DRI?", value = TRUE)
      ),

      # Main panel for displaying the output
      mainPanel(
        width = 8,
        h4("Case Analysis Results"),
        p(
          "The table below shows the pre- and post-treatment data for each case, calculated based on your selected options. Click on any column header to sort the data."
        ),
        hr(),
        # Output for the interactive data table
        DT::dataTableOutput("case_table")
      )
    )),
    # End of TabPanel "Case Analysis"

    # Tab 2: Plot View (Existing content)
    tabPanel("Case Plot View", sidebarLayout(
      sidebarPanel(
        h4("Plot Options"),
        # Input for selecting the case to plot
        selectInput(
          "case_select",
          "Select Case:",
          # Use unique cases from human_data as choices
          choices = unique(deliberr::human_data$case),
          selected = unique(deliberr::human_data$case)[1]
        )
      ),

      mainPanel(
        width = 8,
        h4("Pre- vs. Post-Deliberation Plot"),
        p(
          "This view displays the individual case data for the selected case, showing the difference between pre- and post-deliberation stages."
        ),
        hr(),
        # Output for the plot
        plotOutput("case_plot", height = "700px")
      )
    )),
    # End of TabPanel "Case Plot View"

    # Tab 3: LLM Data Generation (Updated with Clear Data button and Refresh)
    tabPanel("LLM Data", sidebarLayout(
      sidebarPanel(
        h3("LLM Data Generation Controls"),

        # 1. API Key Input
        textInput("api_key", "1. OpenRouter API Key", placeholder = "Enter your key here"),
        actionButton("set_api_key", "Set API Key", class = "btn-primary"),
        br(),
        br(),
        hr(),

        # 2. LLM Variables Input
        h4("LLM Parameters"),
        # CHANGED: Replaced textInput with selectInput for Model ID
        selectInput(
          "model_id",
          "Model ID",
          choices = NULL,
          # Choices will be dynamically set in server.R
          selected = "openai/gpt-3.5-turbo"
        ),
        selectInput("survey_name", "Survey Name", choices = sort(unique(
          deliberr::surveys$name
        ))),
        selectInput("role_uid", "Role UID", choices = sort(deliberr::roles$uid)),
        numericInput(
          "n_iterations",
          "Number of Iterations (n)",
          value = 5,
          min = 1,
          max = 20
        ),
        actionButton("generate_llm_data", "Generate LLM Data", class = "btn-success"),
        hr(),

        # Data Management Controls
        h4("Data Management"),
        # Button to upload LLM data
        fileInput(
          "upload_llm_file",
          "Upload LLM Data (CSV)",
          accept = c("text/csv", "text/comma-separated-values", ".csv")
        ),
        # Button to save LLM data locally
        downloadButton("download_llm_data", "Save LLM Data", class = "btn-info"),
        # Button to clear all LLM data
        actionButton("clear_llm_data", "Clear All LLM Data", class = "btn-danger"),
        # NEW: Button to refresh all LLM data from CSV
        actionButton("refresh_llm_data", "Refresh", class = "btn-warning")
      ),

      mainPanel(
        width = 8,
        h3("LLM Data Generation Results"),
        p(
          "The table below accumulates all generated LLM responses, showing key metadata and cost estimates."
        ),
        hr(),
        # Output for the results table
        DT::dataTableOutput("llm_results_table")
      )
    )),
    # End of TabPanel "LLM Data"

    # Tab 4: LLM Analysis (Updated with Model Filter, Plot Height and Summary Table)
    tabPanel("LLM Analysis", sidebarLayout(
      sidebarPanel(
        h3("LLM Data Visualization Filters"),

        # Filters (Choices dynamically set in server.R)
        selectInput("llm_survey_filter", "Filter by Survey:", choices = sort(unique(
          deliberr::surveys$name
        ))),
        selectInput(
          "llm_model_filter",
          "Filter by Model ID:",
          choices = c("All") # Initial choice; dynamically updated in server
        ),
        # CHANGED: selectizeInput for multi-select
        selectizeInput(
          "llm_role_filter",
          "Filter by Role UID:",
          choices = c("All", sort(deliberr::roles$uid)),
          multiple = TRUE,
          # Allow multiple selections
          options = list(placeholder = 'Select one or more roles')
        ),
        hr(),

        # Plot options
        h4("Plot Options"),
        textInput(
          "llm_plot_title",
          "Plot Title",
          value = "LLM DRI plot" # Default value
        ),
        textInput(
          "llm_plot_suffix",
          "Suffix for Plot Labels (e.g., 'LLM')",
          value = "LLM" # Default value
        )
      ),

      mainPanel(
        width = 8,
        h3("LLM Data Analysis"),
        p(
          "This section visualizes the aggregated Deliberation Reasoning Index (DRI) and provides a summary of the data quality and cost."
        ),
        hr(),
        fluidRow(
          # Column for the Plot (7/12 width)
          column(
            6,
            h4("Aggregated DRI Plot"),
            # Plot height is now max 300px
            plotOutput("llm_plot", height = "auto")
          ),
          # Column for the Summary Table (5/12 width)
          column(
            6,
            h4("Analysis Summary"),
            DT::dataTableOutput("llm_summary_table")
          )
        )
      )
    )) # End of TabPanel "LLM Analysis"
  ) # End of tabsetPanel
)
