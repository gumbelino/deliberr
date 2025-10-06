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
# We'll use dummy data for UI setup if not explicitly provided, but assume they are available
# for the choices based on the user's request, as deliberr is loaded.
if (!exists("surveys")) {
  surveys <- data.frame(name = c("Survey A", "Survey B", "Survey C"))
}
if (!exists("roles")) {
  roles <- data.frame(uid = c("Role 1", "Role 2", "Role 3"))
}

# --- User Interface (UI) Definition ---
fluidPage(
  # App title
  titlePanel("deliberr"),

  # Main area now uses a tabsetPanel for different views
  tabsetPanel(
    # Tab 1: Case Analysis (Existing content)
    tabPanel("Case Analysis",
             sidebarLayout(
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
                 checkboxInput(
                   "adjusted",
                   "Use adjusted DRI?",
                   value = TRUE
                 )
               ),

               # Main panel for displaying the output
               mainPanel(
                 width = 8,
                 h4("Case Analysis Results"),
                 p("The table below shows the pre- and post-treatment data for each case, calculated based on your selected options. Click on any column header to sort the data."),
                 hr(),
                 # Output for the interactive data table
                 DT::dataTableOutput("case_table")
               )
             )
    ), # End of TabPanel "Case Analysis"

    # Tab 2: Plot View (Existing content)
    tabPanel("Case Plot View",
             sidebarLayout(
               sidebarPanel(
                 h4("Plot Options"),
                 # Input for selecting the case to plot
                 selectInput(
                   "case_select",
                   "Select Case:",
                   # Use unique cases from human_data as choices
                   choices = unique(human_data$case),
                   selected = unique(human_data$case)[1]
                 )
               ),

               mainPanel(
                 width = 8,
                 h4("Pre- vs. Post-Deliberation Plot"),
                 p("This view displays the individual case data for the selected case, showing the difference between pre- and post-deliberation stages."),
                 hr(),
                 # Output for the plot
                 plotOutput("case_plot", height = "700px")
               )
             )
    ), # End of TabPanel "Case Plot View"

    # Tab 3: LLM Data Generation (Existing content)
    tabPanel("LLM Data",
             sidebarLayout(
               sidebarPanel(
                 h3("LLM Data Generation Controls"),

                 # 1. API Key Input
                 textInput(
                   "api_key",
                   "1. OpenRouter API Key",
                   placeholder = "Enter your key here"
                 ),
                 actionButton(
                   "set_api_key",
                   "Set API Key",
                   class = "btn-primary"
                 ),
                 br(), br(),
                 hr(),

                 # 2. LLM Variables Input
                 h4("LLM Simulation Parameters"),
                 textInput(
                   "model_id",
                   "Model ID",
                   value = "openai/gpt-3.5-turbo" # Default value
                 ),
                 selectInput(
                   "survey_name",
                   "Survey Name",
                   choices = sort(unique(surveys$name))
                 ),
                 selectInput(
                   "role_uid",
                   "Role UID",
                   choices = sort(roles$uid)
                 ),
                 numericInput(
                   "n_simulations",
                   "Number of Simulations (n)",
                   value = 5,
                   min = 1,
                   max = 20
                 ),
                 actionButton(
                   "generate_llm_data",
                   "Generate LLM Data",
                   class = "btn-success"
                 )
               ),

               mainPanel(
                 width = 8,
                 h3("LLM Data Generation Results"),
                 p("The table below accumulates all generated LLM responses, showing key metadata and cost estimates."),
                 hr(),
                 # 3. Output for the results table
                 DT::dataTableOutput("llm_results_table")
               )
             )
    ), # End of TabPanel "LLM Data"

    # Tab 4: LLM Analysis (NEW content)
    tabPanel("LLM Analysis",
             sidebarLayout(
               sidebarPanel(
                 h3("LLM Data Visualization"),

                 # Filters (Choices dynamically set in server.R)
                 selectInput(
                   "llm_survey_filter",
                   "Filter by Survey:",
                   choices = sort(unique(surveys$name)) # Changed: Removed "All" option
                 ),
                 selectInput(
                   "llm_role_filter",
                   "Filter by Role UID:",
                   choices = c("All", sort(roles$uid))
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
                 h3("Deliberation Reasoning Index (DRI) Plot for LLM Data"),
                 p("This plot visualizes the aggregated Deliberation Reasoning Index (DRI) for valid LLM responses, filtered by the selected survey and role parameters."),
                 hr(),
                 # Output for the plot
                 plotOutput("llm_plot", height = "700px")
               )
             )
    ) # End of TabPanel "LLM Analysis"
  ) # End of tabsetPanel
)
