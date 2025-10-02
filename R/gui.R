


#' GUI for deliberr
#'
#' Launches GUI for deliberr
#'
#' @returns nothing
#' @export
#'
#' @import DT shiny gridExtra dplyr
#'
#' @examples
#'
#' gui()
gui <- function() {

  # --- Run the Application ---
  shiny::shinyApp(ui = ui, server = server, options = list("launch.browser" = TRUE))

}


# --- User Interface (UI) Definition ---
ui <- shiny::fluidPage(
  # App title
  shiny::titlePanel("deliberr"),

  # Main area now uses a tabsetPanel for different views
  shiny::tabsetPanel(
    # Tab 1: Table View (Existing content)
    shiny::tabPanel("Case Analysis",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::h4("Analysis Options"),
                 # Input for selecting the statistical method
                 shiny::selectInput(
                   "method",
                   "Method:",
                   choices = c("wilcox", "t.test"),
                   selected = "wilcox"
                 ),
                 # Input for selecting the alternative hypothesis
                 shiny::selectInput(
                   "alternative",
                   "Alternative Hypothesis:",
                   choices = c("greater", "two.sided", "less"),
                   selected = "greater"
                 ),
                 # Checkbox for using adjusted p-values
                 shiny::checkboxInput(
                   "adjusted",
                   "Use adjusted DRI?",
                   value = TRUE
                 )
               ),

               # Main panel for displaying the output
               shiny::mainPanel(
                 width = 8,
                 shiny::h4("Case Analysis Results"),
                 shiny::p("The table below shows the pre- and post-treatment data for each case, calculated based on your selected options. Click on any column header to sort the data."),
                 shiny::hr(),
                 # Output for the interactive data table
                 DT::dataTableOutput("case_table")
               )
             )
    ), # End of TabPanel "Table Analysis"

    # Tab 2: Plot View (New content)
    shiny::tabPanel("Case Plot View",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::h4("Plot Options"),
                 # Input for selecting the case to plot
                 shiny::selectInput(
                   "case_select",
                   "Select Case:",
                   # Use unique cases from human_data as choices
                   choices = unique(deliberr::human_data$case),
                   selected = unique(deliberr::human_data$case)[1]
                 )
               ),

               shiny::mainPanel(
                 width = 8,
                 shiny::h4("Pre- vs. Post-Deliberation Plot"),
                 shiny::p("This view displays the individual case data for the selected case, showing the difference between pre- and post-deliberation stages."),
                 shiny::hr(),
                 # Output for the plot - **Increased height here**
                 shiny::plotOutput("case_plot", height = "700px")
               )
             )
    ) # End of TabPanel "Case Plot View"
  ) # End of tabsetPanel
)

# --- Server Logic Definition ---
server <- function(input, output) {

  # --- Reactive Data Generation for Table ---
  # ( ... existing reactive_case_dri_df code ... )
  reactive_case_dri_df <- reactive({
    # Initialize an empty list to store results for each case
    case_dri_list <- list()

    # Loop through each unique case and apply the selected parameters
    for (case in unique(deliberr::human_data$case)) {
      case_dri_list[[length(case_dri_list) + 1]] <- get_dri_case(
        case,
        method = input$method,
        adjusted = input$adjusted,
        alternative = input$alternative
      )
    }

    # Combine the list of data frames into a single data frame
    bind_rows(case_dri_list)
  })

  # Render the interactive data table
  output$case_table <- DT::renderDataTable({
    data_to_display <- reactive_case_dri_df()

    datatable_obj <- DT::datatable(
      data_to_display, # Use the reactive data frame
      options = list(
        pageLength = -1, # Show all rows by default
        lengthChange = FALSE, # Hide the "Show X entries" dropdown
        autoWidth = TRUE,
        language = list(
          info = "Showing _TOTAL_ entries" # Custom info text
        )
      ),
      rownames = FALSE,
      filter = 'none',
      class = 'cell-border stripe'
    ) %>%
      # 1. Apply fixed-point rounding to 2 decimal places
      DT::formatSignif(
        columns = which(sapply(data_to_display, is.numeric)),
        digits = 2
      ) %>%
      # 2. Apply conditional text coloring based on the 'delta' column
      DT::formatStyle(
        columns = 'delta',
        valueColumns = 'delta',
        # Use 'styleInterval' to apply green if > 0, red otherwise
        color = DT::styleInterval(
          # The intervals break point is 0
          cuts = 0,
          # The colors are red (<= 0) and green (> 0)
          values = c('red', 'green')
        ),
        # Also apply bold font for emphasis
        fontWeight = DT::styleInterval(0, c('bold', 'bold'))
      )

    return(datatable_obj)
  })

  # ( ... existing reactive_case_plot and output$case_plot code ... )
  # --- Reactive Plot Generation ---
  reactive_case_plot <- reactive({
    # Ensure a case is selected
    req(input$case_select)

    # Filter data for the selected case
    data_pre <- deliberr::human_data %>% filter(case == input$case_select, stage_id == 1)
    data_post <- deliberr::human_data %>% filter(case == input$case_select, stage_id == 2)

    # Calculate indices
    ic_pre <- get_dri_ic(data_pre)
    ic_post <- get_dri_ic(data_post)

    # Calculate DRI
    dri_pre <- get_dri(ic_pre)
    dri_post <- get_dri(ic_post)

    # Generate plots
    plot_pre <- plot_dri_ic(ic_pre, title = input$case_select, suffix = "pre", dri = dri_pre)
    plot_post <- plot_dri_ic(ic_post, title = input$case_select, suffix = "post", dri = dri_post)

    data_dri <- data.frame(
      Time = factor(c("Pre", "Post"), levels = c("Pre", "Post")), # Ensure 'Pre' comes before 'Post'
      DRI = c(dri_pre, dri_post)
    )

    # 1. Calculate the Delta (Post - Pre)
    # Assuming dri_pre and dri_post are single numeric values
    delta_value <- dri_post - dri_pre

    color <- case_when(
      delta_value < 0 ~ "#B91C1C",
      delta_value > 0 ~ "#1CB91C",
      .default = "#1C1CB9"
    )

    # 2. Determine the coordinates for the annotation
    # Y-coordinate: The average of pre and post DRI values
    avg_dri <- (dri_pre + dri_post) / 2
    # X-coordinate: The middle point between 'Pre' and 'Post' (which is index 1.5 on the x-axis)
    # The text label for the delta
    delta_label <- paste0("delta = ", round(delta_value, 3))

    # 3. Generate the line plot with the Delta annotation
    delta_plot <- ggplot(data_dri, aes(x = Time, y = DRI, group = 1)) +

      # Add the line connecting the 'Pre' and 'Post' points
      geom_line(color = color, size = 1.5) +

      # Add points for clarity
      geom_point(color = color, size = 5) +

      # Set the Y-axis scale from 0 to 1
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +

      # Label the Pre/Post DRI values
      geom_text(aes(label = round(DRI, 3)),
                vjust = ifelse(data_dri$Time == "Pre", -1.5, 2.5),
                color = color,
                size = 5) +

      # 📣 NEW: Add the Delta value annotation
      annotate("text",
               x = 1.5, # Position in the middle of 'Pre' (1) and 'Post' (2)
               y = avg_dri, # Position vertically near the line center
               label = delta_label,
               hjust = 0.5, # Center the text horizontally
               vjust = ifelse(delta_value >= 0, -1.5, 2.5), # Adjust vertical position to be above/below the line
               color = color, # Use a contrasting color (e.g., dark red)
               size = 5,
               fontface = "bold"
      ) +

      # Add informative labels and title
      labs(
        title = "Change in DRI (Pre vs. Post Deliberation)",
        x = "Deliberation Stage",
        y = "DRI"
      ) +

      # Apply a clean, professional theme
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        # Remove grid lines on the X-axis for a cleaner look
        panel.grid.major.x = element_blank()
      )


    grid.arrange(plot_pre, plot_post, delta_plot, ncol = 2,
                 layout_matrix = cbind(c(1,3), c(2,3)))

  })

  # Render the plot - **Added explicit width/height to renderPlot**
  output$case_plot <- renderPlot({
    reactive_case_plot()
  }, height = 700, width = 800) # Set height/width in pixels
}
