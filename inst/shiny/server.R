#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load necessary libraries
library(shiny)
library(deliberr)
library(dplyr)
library(ggplot2)
library(DT)
library(gridExtra)
library(deliberr) # Ensure deliberr is loaded for LLM functions

function(input, output, session) {

  # --- Existing Reactive Data Generation for Table ---
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

  # Existing Render the interactive data table
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

  # --- Existing Reactive Plot Generation ---
  reactive_case_plot <- reactive({
    # Ensure a case is selected
    req(input$case_select)

    # Filter data for the selected case
    data_pre <- human_data %>% filter(case == input$case_select, stage_id == 1)
    data_post <- human_data %>% filter(case == input$case_select, stage_id == 2)

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
    delta_value <- dri_post - dri_pre

    color <- case_when(
      delta_value < 0 ~ "#B91C1C",
      delta_value > 0 ~ "#1CB91C",
      .default = "#1C1CB9"
    )

    # 2. Determine the coordinates for the annotation
    avg_dri <- (dri_pre + dri_post) / 2
    delta_label <- paste0("delta = ", round(delta_value, 3))

    # 3. Generate the line plot with the Delta annotation
    delta_plot <- ggplot(data_dri, aes(x = Time, y = DRI, group = 1)) +
      geom_line(color = color, size = 1.5) +
      geom_point(color = color, size = 5) +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
      geom_text(aes(label = round(DRI, 3)),
                vjust = ifelse(data_dri$Time == "Pre", -1.5, 2.5),
                color = color,
                size = 5) +
      annotate("text",
               x = 1.5,
               y = avg_dri,
               label = delta_label,
               hjust = 0.5,
               vjust = ifelse(delta_value >= 0, -1.5, 2.5),
               color = color,
               size = 5,
               fontface = "bold"
      ) +
      labs(
        title = "Change in DRI (Pre vs. Post Deliberation)",
        x = "Deliberation Stage",
        y = "DRI"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major.x = element_blank()
      )

    grid.arrange(plot_pre, plot_post, delta_plot, ncol = 2,
                 layout_matrix = cbind(c(1,3), c(2,3)))

  })

  # Existing Render the plot
  output$case_plot <- renderPlot({
    reactive_case_plot()
  }, height = 700, width = 800)


  # --- LLM Data Generation Logic ---

  # 1. Reactive value to store the accumulated LLM results (POSIXct for date/time)
  llm_results <- reactiveVal(
    data.frame(
      # The initial structure must contain all necessary columns,
      # including those required by get_dri_ic(), e.g., role, stage, score_1, score_2.
      # For simplicity, we initialize with a minimal set, assuming new_data will add the rest.
      created_at_utc = as.POSIXct(character()),
      provider = character(),
      model = character(),
      survey = character(),
      role_uid = character(),
      time_s = numeric(),
      est_cost_usd = numeric(),
      is_valid = logical(),
      invalid_reason = character(),
      stringsAsFactors = FALSE
    )
  )

  # Observer for setting the API key
  observeEvent(input$set_api_key, {
    # Check if the key is provided
    if (nchar(input$api_key) > 0) {
      Sys.setenv(OPENROUTER_API_KEY = input$api_key)
      showNotification("OpenRouter API Key has been set.", type = "message")
    } else {
      showNotification("Please enter an API Key.", type = "warning")
    }
  })


  # Event observer for generating LLM data
  observeEvent(input$generate_llm_data, {
    # Ensure API key is set before proceeding
    if (Sys.getenv("OPENROUTER_API_KEY") == "") {
      showNotification("Error: Please set the OpenRouter API Key first.", type = "error")
      return()
    }

    # Ensure n is valid
    if (input$n_simulations < 1 || input$n_simulations > 20) {
      showNotification("Error: 'n' must be between 1 and 20.", type = "error")
      return()
    }

    id <- showNotification("Generating LLM data... this may take a moment.", type = "default", duration = NULL)

    # Define the core metadata columns for validation
    METADATA_COLS_FOR_VALIDATION <- c("created_at_utc", "provider", "model", "survey", "role_uid", "time_s", "est_cost_usd", "is_valid", "invalid_reason")

    # Call the LLM function within a tryCatch block to handle potential API errors
    tryCatch({
      # Call the user-requested function
      new_data <- deliberr::get_dri_llm_response(
        model_id = input$model_id,
        survey_name = input$survey_name,
        role_uid = input$role_uid,
        n = input$n_simulations
      )

      # Stop the "Generating" notification
      removeNotification(id = id)

      # Append the new data to the reactive value
      # Check for required metadata columns and if data was returned
      if (nrow(new_data) > 0 && all(METADATA_COLS_FOR_VALIDATION %in% names(new_data))) {

        # --- FIX: Store ALL columns for plotting, not just metadata ---
        new_data_full <- new_data %>%
          # Explicitly convert created_at_utc to POSIXct
          mutate(created_at_utc = as.POSIXct(created_at_utc))

        # Get current data and bind new data rows
        updated_data <- bind_rows(llm_results(), new_data_full)

        # Update the reactive value
        llm_results(updated_data)
        # --- END FIX ---

        showNotification(paste("Successfully generated and added", nrow(new_data), "new data points."), type = "message")

      } else {
        showNotification("LLM response did not contain expected data or columns.", type = "error")
      }

    }, error = function(e) {
      # Stop the "Generating" notification
      removeNotification(id = id)
      # Display the error message
      showNotification(paste("LLM Generation Failed:", conditionMessage(e)), type = "error", duration = 10)
    })
  })

  # Render the accumulated LLM results table
  output$llm_results_table <- DT::renderDataTable({
    data_to_display <- llm_results()

    # Define the columns to show in the table (metadata only)
    DISPLAY_METADATA_COLS <- c("created_at_utc", "provider", "model", "survey", "role_uid", "time_s", "est_cost_usd", "is_valid", "invalid_reason")

    # --- FIX: Filter the data only for display in the table ---
    data_for_datatable <- data_to_display %>%
      select(any_of(DISPLAY_METADATA_COLS))
    # --- END FIX ---

    DT::datatable(
      data_for_datatable, # Use the filtered data for display
      options = list(
        # Set default sort on created_at_utc (newest first)
        order = list(0, 'desc'),
        pageLength = 10,
        scrollX = TRUE, # Allow horizontal scrolling for better viewing of all columns
        autoWidth = FALSE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      # Format numeric columns for readability
      DT::formatSignif(
        columns = c('time_s', 'est_cost_usd'),
        digits = 4
      ) %>%
      # Highlight invalid rows
      DT::formatStyle(
        columns = 'is_valid',
        target = 'row',
        backgroundColor = DT::styleEqual(FALSE, '#ffdddd') # Light red background for invalid rows
      )
  })

  # --- LLM Analysis Plotting Logic ---

  # Dynamic Filter Updates for LLM Analysis Tab
  observe({
    # Defensive check: ensure llm_results() is not empty before filtering
    if (nrow(llm_results()) == 0) {
      # When no data is present, set survey choices to empty and role choices to only "All"
      updateSelectInput(session, "llm_survey_filter", choices = character(0), selected = NULL)
      updateSelectInput(session, "llm_role_filter", choices = "All", selected = "All")
      return()
    }

    # Filter for valid data before extracting unique choices
    current_data <- llm_results() %>% filter(is_valid == TRUE)

    # 1. Update Survey choices (REMOVING "All" and ensuring a selection is made)
    survey_choices <- sort(unique(current_data$survey))
    # Preserve current selection if possible, otherwise default to the first one
    selected_survey <- if (input$llm_survey_filter %in% survey_choices) input$llm_survey_filter else survey_choices[1]
    updateSelectInput(session, "llm_survey_filter", choices = survey_choices, selected = selected_survey)

    # 2. Update Role choices (Keeping "All")
    role_choices <- c("All", sort(unique(current_data$role_uid)))
    updateSelectInput(session, "llm_role_filter", choices = role_choices)
  })

  # Reactive expression to filter, calculate IC and DRI for plotting
  reactive_llm_plot_data <- reactive({
    data <- llm_results()

    # CRUCIAL: Require that a survey is selected before proceeding.
    req(input$llm_survey_filter)

    # 1. Filter only valid data
    # Defensive check: return NULL if no data is present initially
    if (nrow(data) == 0) {
      return(NULL)
    }

    data <- data %>% filter(is_valid == TRUE)

    # Defensive check: return NULL if no VALID data remains
    if (nrow(data) == 0) {
      return(NULL)
    }

    # 2. Filter by survey (Now guaranteed to have a single value due to req())
    data <- data %>% filter(survey == input$llm_survey_filter)

    # 3. Filter by role
    if (input$llm_role_filter != "All") {
      data <- data %>% filter(role_uid == input$llm_role_filter)
    }

    # Defensive check: return NULL if data is filtered down to empty set
    if (nrow(data) == 0) {
      return(NULL)
    }

    # 4. Calculate IC and DRI
    ic_llm <- deliberr::get_dri_ic(data)
    dri <- deliberr::get_dri(ic_llm)

    list(ic = ic_llm, dri = dri)
  })

  # Render the LLM Analysis plot with improved error handling
  output$llm_plot <- renderPlot({
    plot_data <- reactive_llm_plot_data()

    if (is.null(plot_data)) {
      # Return a placeholder plot or message if no valid data is available
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid LLM data available for selected filters.") + theme_void())
    }

    # Use user-defined title and suffix, with specified defaults
    plot_title <- input$llm_plot_title
    if (is.null(plot_title) || nchar(plot_title) == 0) {
      plot_title <- "LLM DRI plot"
    }

    plot_suffix <- input$llm_plot_suffix
    if (is.null(plot_suffix) || nchar(plot_suffix) == 0) {
      plot_suffix <- "LLM" # Default
    }

    # Use tryCatch for robust error reporting
    tryCatch({
      # This is the actual plotting function
      deliberr::plot_dri_ic(
        ic = plot_data$ic,
        title = plot_title,
        suffix = plot_suffix,
        dri = plot_data$dri
      )
    }, error = function(e) {
      # If plotting fails, return a custom ggplot with the R error message
      error_message <- paste("Plotting Failed (R Error):", conditionMessage(e))
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = error_message, color = "red", size = 5) +
               theme_void() +
               labs(title = "Plot Generation Error")
      )
    })

  }, height = 700) # Consistent plot height
}
