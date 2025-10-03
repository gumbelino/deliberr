#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

# --- User Interface (UI) Definition ---
fluidPage(
  # App title
  titlePanel("deliberr"),

  # Main area now uses a tabsetPanel for different views
  tabsetPanel(
    # Tab 1: Table View (Existing content)
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
    ), # End of TabPanel "Table Analysis"

    # Tab 2: Plot View (New content)
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
                 # Output for the plot - **Increased height here**
                 plotOutput("case_plot", height = "700px")
               )
             )
    ) # End of TabPanel "Case Plot View"
  ) # End of tabsetPanel
)

