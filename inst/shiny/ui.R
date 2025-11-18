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

    # Add this as the FIRST tabPanel in your tabsetPanel() in ui.R
    # Place it before the "Case Analysis" tab

    tabPanel("Home",
             fluidPage(
               style = "background-color: #f8f9fa; padding-top: 20px;",

               # Header Section
               div(
                 style = "text-align: center; padding: 40px 20px; background-color: white; border-radius: 8px; margin-bottom: 30px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                 h1("Welcome to deliberr", style = "color: #2c3e50; margin-bottom: 10px;"),
                 p("Analyze human, LLM, and combined deliberation data through the Deliberation Reasoning Index (DRI)",
                   style = "font-size: 18px; color: #555; margin: 0;")
               ),

               # Quick Start Section
               div(
                 style = "background-color: #e8f4f8; padding: 20px; border-left: 4px solid #0288d1; border-radius: 4px; margin-bottom: 30px;",
                 h3("Quick Start", style = "margin-top: 0;"),
                 p("To get started with LLM data generation, you'll need an OpenRouter API key:"),
                 tags$ol(
                   tags$li("Visit ", a(href="https://openrouter.ai/", target="_blank", "openrouter.ai"), " and create an account"),
                   tags$li("Generate an API key from your account settings"),
                   tags$li("Use the API key in the ", strong("LLM Data"), " tab to generate LLM responses"),
                   tags$li("Analyze results in the ", strong("LLM Analysis"), " or ", strong("Human+LLM Analysis"), " tabs")
                 )
               ),

               # Main Features Grid
               h2("Features", style = "color: #2c3e50; margin-bottom: 20px;"),

               fluidRow(
                 # Feature 1: Case Analysis
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("Case Analysis", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Analyze human deliberation data collected from case studies."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("View pre- and post-deliberation DRI scores for each case"),
                                  tags$li("Statistical testing (Wilcoxon or t-test) to compare conditions"),
                                  tags$li("Calculate pre/post differences (delta) and significance"),
                                  tags$li("Upload custom human data with standardized columns"),
                                  tags$li("Filter by statistical method and adjustment options")
                          ),
                          p(style = "font-size: 0.9em; color: #666; margin: 0;",
                            "Requires CSV with: survey, case, stage_id, pnum, C1:C50, P1:P10")
                        )
                 ),

                 # Feature 2: Case Plot View
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("Case Plot View", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Visualize individual case data and deliberation changes."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("View side-by-side pre/post DRI scatter plots"),
                                  tags$li("See aggregated DRI change visualization"),
                                  tags$li("Select specific cases to inspect"),
                                  tags$li("Color-coded delta values (green for increase, red for decrease)")
                          )
                        )
                 )
               ),

               fluidRow(
                 # Feature 3: Surveys
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("Surveys", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Upload and manage survey data for deliberation studies."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("Upload custom surveys in CSV format"),
                                  tags$li("View all available surveys in an organized table"),
                                  tags$li("Newly uploaded surveys are highlighted for easy identification"),
                                  tags$li("Surveys automatically appear in all dropdown menus across the app"),
                                  tags$li("Support for statement-based surveys with configurable scales")
                          ),
                          p(style = "font-size: 0.9em; color: #666; margin: 0;",
                            "Requires CSV with: type, order, statement, name, scale_max, q_method")
                        )
                 ),

                 # Feature 4: LLM Roles
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("LLM Roles", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Create and manage custom roles for LLM simulations."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("Define custom roles with descriptions (max 100 words)"),
                                  tags$li("View all available default and custom roles"),
                                  tags$li("Upload roles from CSV files"),
                                  tags$li("Download custom roles for sharing"),
                                  tags$li("Each role includes: UID, name, description, and type")
                          ),
                          p(style = "font-size: 0.9em; color: #666; margin: 0;",
                            "Custom roles are highlighted in yellow and saved locally")
                        )
                 )
               ),

               fluidRow(
                 # Feature 5: LLM Data
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("LLM Data Generation", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Generate LLM responses using OpenRouter API."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("Select from multiple LLM models (OpenAI, Anthropic, etc.)"),
                                  tags$li("Choose survey and custom role for LLM"),
                                  tags$li("Run 1-20 iterations per session"),
                                  tags$li("Track cost and generation time"),
                                  tags$li("Upload, download, and clear LLM data"),
                                  tags$li("View validity status and error messages")
                          ),
                          p(style = "font-size: 0.9em; color: #666; margin: 0;",
                            "Requires OpenRouter API key")
                        )
                 ),

                 # Feature 6: LLM Analysis
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("LLM Analysis", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Analyze and visualize aggregated LLM-generated data."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("Filter by survey, model, and role"),
                                  tags$li("View aggregated DRI plot for LLM responses"),
                                  tags$li("Summary statistics: total iterations, success rate, cost, time"),
                                  tags$li("Calculate Cronbach's alpha for reliability"),
                                  tags$li("Customize plot titles and labels")
                          )
                        )
                 )
               ),

               fluidRow(
                 # Feature 7: Human+LLM Analysis
                 column(6,
                        div(
                          style = "background-color: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); height: 100%;",
                          h3("Human+LLM Analysis", style = "margin-top: 0; margin-bottom: 10px; color: #2c3e50;"),
                          p(style = "margin-bottom: 12px;", "Compare human data with LLM-generated responses."),
                          tags$ul(style = "margin-bottom: 12px;",
                                  tags$li("Select specific cases and deliberation stages"),
                                  tags$li("Choose LLM model and role(s) for comparison"),
                                  tags$li("View human-only DRI plot"),
                                  tags$li("View combined human+LLM DRI plot"),
                                  tags$li("Analyze differences between human and LLM reasoning")
                          )
                        )
                 )
               ),

               # Workflow Section
               h2("Typical Workflow", style = "color: #2c3e50; margin-bottom: 20px;"),
               div(
                 style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
                 tags$ol(
                   tags$li(strong("Start with Case Analysis:"), " Explore existing human deliberation data"),
                   tags$li(strong("Create Roles:"), " Define custom roles in the LLM Roles tab if needed"),
                   tags$li(strong("Upload Surveys:"), " Add custom surveys using the Surveys tab if needed"),
                   tags$li(strong("Generate LLM Data:"), " Use the LLM Data tab to generate responses from your chosen model and role"),
                   tags$li(strong("Analyze LLM Results:"), " Use LLM Analysis to visualize and evaluate the generated data"),
                   tags$li(strong("Compare Human+LLM:"), " Use Human+LLM Analysis to compare human responses with LLM-generated data"),
                   tags$li(strong("Export & Share:"), " Download data and roles for further analysis or sharing")
                 )
               ),

               # Data Requirements Section
               h2("Data Format Requirements", style = "color: #2c3e50; margin-bottom: 20px;"),
               fluidRow(
                 column(4,
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
                          h4("Human Data (CSV)", style = "margin-top: 0;"),
                          p(style = "font-size: 0.9em;", strong("Required columns:")),
                          code(style = "display: block; padding: 10px; background-color: #f5f5f5; margin: 10px 0; border-radius: 4px; font-size: 0.85em;",
                               "survey, case, stage_id,
pnum, C1:C50, P1:P10")
                        )
                 ),
                 column(4,
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
                          h4("Surveys (CSV)", style = "margin-top: 0;"),
                          p(style = "font-size: 0.9em;", strong("Required columns:")),
                          code(style = "display: block; padding: 10px; background-color: #f5f5f5; margin: 10px 0; border-radius: 4px; font-size: 0.85em;",
                               "type, order, statement,
name, scale_max, q_method")
                        )
                 ),
                 column(4,
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
                          h4("Custom Roles (CSV)", style = "margin-top: 0;"),
                          p(style = "font-size: 0.9em;", strong("Required columns:")),
                          code(style = "display: block; padding: 10px; background-color: #f5f5f5; margin: 10px 0; border-radius: 4px; font-size: 0.85em;",
                               "uid, type, role,
description")
                        )
                 )
               ),

               br(),
               br()
             )
    ),


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

    tabPanel("Surveys", sidebarLayout(
      sidebarPanel(
        h3("Upload New Survey"),

        p("Upload a CSV file with survey questions. The file must include these required columns:",
          style = "font-size: 0.9em;"),
        code(
          style = "display: block; padding: 10px; background-color: #f5f5f5; margin: 10px 0; border-radius: 4px;",
          "type, order, statement, name, scale_max, q_method"
        ),

        p("Survey names in the 'name' column should be unique and will be used to identify the survey across the app.",
          style = "font-size: 0.9em;"),

        hr(),

        fileInput(
          "upload_survey_file",
          "Upload Survey (CSV)",
          accept = c("text/csv", "text/comma-separated-values", ".csv")
        ),

        p(
          style = "font-size: 0.85em; color: #666;",
          "Once uploaded, new surveys will be available in all survey dropdowns across the app."
        )
      ),

      mainPanel(
        width = 8,
        h3("Available Surveys"),
        p(
          "This table shows all available survey questions."
        ),
        hr(),
        DT::dataTableOutput("surveys_table")
      )
    )),

    # NEW Tab: LLM Roles Management
    tabPanel("LLM Roles", sidebarLayout(
      sidebarPanel(
        h3("Create Custom Role"),

        p("Define a new role for LLM simulations. Here is the template the system
      will use as instruction to the LLM:"),
        htmlOutput("role_template_preview"),
        hr(),

        # Custom Role Input Form
        textInput("new_role_uid", "UID (3-10 chars, unique identifier):", value = ""),
        # selectInput("new_role_article", "Article:", choices = NULL), # Choices updated in server
        textInput(
          "new_role_name",
          "Role name (3-20 chars, letters/hyphens/spaces only):",
          value = ""
        ),
        textAreaInput(
          "new_role_description",
          "Description (max 100 words, letters/hyphens/spaces only):",
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
        selectInput("llm_survey_filter", "Filter by survey", choices = sort(unique(
          deliberr::surveys$name
        ))),
        selectizeInput(
          "llm_model_filter",
          "Filter by model",
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

        # 3. Model Selection (multi-select)
        h4("LLM Data Options"),
        selectizeInput(
          "hlm_model_select",
          "Model",
          choices = NULL, # Dynamically populated in server
          multiple = TRUE,
          options = list(placeholder = 'Select one or more models')
        ),

        # 4. Role Selection (multi-select)
        selectizeInput(
          "hlm_role_select",
          "Role(s)",
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
