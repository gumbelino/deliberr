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
  roles <- deliberr::roles # Use the initial roles dataframe from deliberr
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
        checkboxInput("adjusted", "Use adjusted DRI?", value = TRUE),

        hr(),

        # NEW: Upload Custom Human Data
        h4("Upload Custom Data"),
        p(
          style = "font-size: 0.9em;",
          "Upload a CSV file with columns: survey, case, stage_id, pnum, C1:C50, P1:P10"
        ),
        fileInput(
          "upload_human_data",
          "Upload Human Data (CSV)",
          accept = c("text/csv", "text/comma-separated-values", ".csv")
        )
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

    # NEW Tab: LLM Roles Management
    tabPanel("LLM Roles", sidebarLayout(
      sidebarPanel(
        h3("Create Custom Role"),

        p("Define a new role for LLM simulations. Here is the template the system
      will use as instruction to the LLM:"),
        htmlOutput("role_template_preview"),
        hr(),

        # Custom Role Input Form
        textInput("new_role_uid", "UID (3 characters, unique):", value = ""),
        # selectInput("new_role_article", "Article:", choices = NULL), # Choices updated in server
        textInput(
          "new_role_name",
          "Role Name (max 10 chars, letters/hyphens only):",
          value = ""
        ),
        textAreaInput(
          "new_role_description",
          "Description (max 100 words, letters/hyphens only):",
          value = "",
          rows = 3
        ),

        actionButton("add_custom_role", "Add Role", class = "btn-success"),

        hr(),

        # Custom Roles Management
        h4("Custom Roles Management"),
        fileInput(
          "upload_custom_roles",
          "Upload Custom Roles (CSV)",
          accept = c("text/csv", "text/comma-separated-values", ".csv")
        ),
        downloadButton("download_custom_roles", "Download Custom Roles", class = "btn-info"),
        br(),
        br(),
        p(
          style = "font-size: 0.9em; color: #666;",
          "Note: Only custom roles (type='custom') will be downloaded. Custom roles are highlighted in yellow in the table."
        )
      ),

      mainPanel(
        width = 8,
        h3("Existing and Custom Roles"),
        p("Visualization of all available roles for LLM simulation. Custom roles are highlighted in yellow."),
        hr(),
        # Output for the reactive roles table
        DT::dataTableOutput("roles_table")
      )
    )),
    # End of TabPanel "LLM Roles"

    # Tab 3: LLM Data Generation (Modified for dynamic role choices)
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
        selectInput("survey_name", "Survey", choices = sort(unique(
          deliberr::surveys$name
        ))),
        # MODIFIED: Initial choices set to NULL, populated by server's reactive_roles
        selectInput("role_uid", "Role", choices = NULL),
        numericInput(
          "n_iterations",
          "Number of iterations (n)",
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
        selectInput("llm_survey_filter", "Filter by survey:", choices = sort(unique(
          deliberr::surveys$name
        ))),
        selectizeInput(
          "llm_model_filter",
          "Filter by model:",
          choices = c("all"), # Initial choice; dynamically updated in server
          multiple = TRUE,
          options = list(placeholder = 'Select one or more models')
        ),
        # CHANGED: selectizeInput for multi-select
        selectizeInput(
          "llm_role_filter",
          "Filter by role",
          choices = c("all", sort(deliberr::roles$uid)),
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
        ),

        # # NEW: Human Data Inclusion Options
        # hr(),
        # h4("Human Data"),
        # selectInput(
        #   "include_human_data",
        #   "Include human data:",
        #   choices = c(
        #     "none" = "none",
        #     "pre-deliberation" = "pre",
        #     "post-deliberation" = "post"
        #   ),
        #   selected = "none" # Default
        # ),

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
    )), # End of TabPanel "LLM Analysis"

    # Tab 5: Human+LLM Analysis (NEW)
    tabPanel("Human+LLM Analysis", sidebarLayout(
      sidebarPanel(
        h3("Combine Human and LLM Data"),

        # 1. Case Selection
        h4("Human Data Options"),
        selectInput(
          "hlm_case_select",
          "Case (survey)",
          choices = NULL # Dynamically populated in server
        ),

        # 2b. Stage Selection
        selectInput(
          "hlm_stage_select",
          "Survey stage",
          choices = c("pre-deliberation" = 1, "post-deliberation" = 2),
          selected = 1
        ),
        textOutput("hlm_human_count"),

        hr(),

        # 3. Model Selection
        h4("LLM Data Options"),
        selectInput(
          "hlm_model_select",
          "Model",
          choices = NULL # Dynamically populated in server
        ),

        # 4. Role Selection (multi-select)
        selectizeInput(
          "hlm_role_select",
          "Role(s):",
          choices = NULL, # Dynamically populated in server
          multiple = TRUE,
          options = list(placeholder = 'Select one or more roles')
        ),
        textOutput("hlm_llm_count"),


        hr(),

        # 5. Plot options
        h4("Plot Options"),
        textInput(
          "hlm_plot_title_human",
          "Human-Only Plot Title",
          value = "Human Data" # Default value
        ),
        textInput(
          "hlm_plot_title_combined",
          "Human+LLM Plot Title",
          value = "Human + LLM Data" # Default value
        ),

      ),

      mainPanel(
        width = 8,
        h3("Human vs. Human+LLM Analysis"),
        p(
          "This view displays the Deliberation Reasoning Index (DRI) for the selected case comparing human-only data with combined human+LLM data."
        ),
        hr(),
        # Output for the plots
        plotOutput("hlm_comparison_plot", height = "700px")
      )
    ))
    # End of TabPanel "Human+LLM Analysis"

  ) # End of tabsetPanel
)
