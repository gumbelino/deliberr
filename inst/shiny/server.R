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
library(readr)
library(gridExtra)

# --- Data Preparation for Model Dropdown (using deliberr::get_model_ids) ---
# NOTE: get_model_ids is called outside the server function for one-time initialization.
model_df <- deliberr::get_model_ids()

# 1. Create the full model ID string ("provider/model")
model_df <- model_df %>%
  mutate(full_model_id = paste0(provider, "/", model))

# 2. Create a named list grouped by provider for the selectInput
model_choices_list <- model_df %>%
  group_by(provider) %>%
  # Create a named vector for each provider group where value=name=full_model_id
  summarise(choices = list(setNames(full_model_id, full_model_id)), .groups = 'drop') %>%
  # Convert the data frame back into the final named list
  pull(choices, name = provider)

# Set the desired default model
default_model <- "openai/gpt-3.5-turbo"

# --- Server Logic Start ---
function(input, output, session) {
  # Define the initial empty data frame structure for reuse
  # This structure must contain all necessary columns for deliberr functions
  initial_llm_data_structure <- data.frame()

  # 1. Reactive value to store the accumulated LLM results (POSIXct for date/time)
  llm_results <- reactiveVal(initial_llm_data_structure)

  # Function to handle loading data from llm_data.csv (Reusable for startup and refresh)
  load_llm_data <- function() {
    if (file.exists("llm_data.csv")) {
      tryCatch({
        # Use read_csv for robust reading of the data frame
        data <- read_csv("llm_data.csv", show_col_types = FALSE)

        if (nrow(data) > 0) {
          llm_results(data)
          showNotification(paste("Loaded", nrow(data), "records from llm_data.csv."),
                           type = "message")
        } else {
          llm_results(initial_llm_data_structure)
          showNotification("llm_data.csv is empty. Starting fresh.", type = "warning")
        }
      }, error = function(e) {
        llm_results(initial_llm_data_structure)
        showNotification(
          paste(
            "Error loading llm_data.csv:",
            conditionMessage(e),
            ". Starting fresh."
          ),
          type = "error"
        )
      })
    } else {
      llm_results(initial_llm_data_structure)
      showNotification("llm_data.csv not found. Starting fresh.", type = "default")
    }
  }

  # ** INITIAL LOAD: Load existing data when the application starts **
  load_llm_data()

  # Initialize the model dropdown with the prepared choices
  observe({
    updateSelectInput(session,
                      "model_id",
                      choices = model_choices_list,
                      selected = default_model)
  }) # Run only once on startup

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
      data_to_display,
      # Use the reactive data frame
      options = list(
        pageLength = -1,
        # Show all rows by default
        lengthChange = FALSE,
        # Hide the "Show X entries" dropdown
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
      DT::formatSignif(columns = which(sapply(data_to_display, is.numeric)), digits = 2) %>%
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
    # FIX: Corrected assignment operator (=) to equality operator (==)
    data_post <- human_data %>% filter(case == input$case_select, stage_id == 2)

    # Calculate indices
    ic_pre <- get_dri_ic(data_pre)
    ic_post <- get_dri_ic(data_post)

    # Calculate DRI
    dri_pre <- get_dri(ic_pre)
    dri_post <- get_dri(ic_post)

    # Generate plots
    plot_pre <- plot_dri_ic(
      ic_pre,
      title = input$case_select,
      suffix = "pre",
      dri = dri_pre
    )
    plot_post <- plot_dri_ic(
      ic_post,
      title = input$case_select,
      suffix = "post",
      dri = dri_post
    )

    data_dri <- data.frame(
      Time = factor(c("Pre", "Post"), levels = c("Pre", "Post")),
      # Ensure 'Pre' comes before 'Post'
      DRI = c(dri_pre, dri_post)
    )

    # 1. Calculate the Delta (Post - Pre)
    delta_value <- dri_post - dri_pre

    color <- case_when(delta_value < 0 ~ "#B91C1C",
                       delta_value > 0 ~ "#1CB91C",
                       .default = "#1C1CB9")

    # 2. Determine the coordinates for the annotation
    avg_dri <- (dri_pre + dri_post) / 2
    delta_label <- paste0("delta = ", round(delta_value, 3))

    # 3. Generate the line plot with the Delta annotation
    delta_plot <- ggplot(data_dri, aes(x = Time, y = DRI, group = 1)) +
      geom_line(color = color, size = 1.5) +
      geom_point(color = color, size = 5) +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
      geom_text(
        aes(label = round(DRI, 3)),
        vjust = ifelse(data_dri$Time == "Pre", -1.5, 2.5),
        color = color,
        size = 5
      ) +
      annotate(
        "text",
        x = 1.5,
        y = avg_dri,
        label = delta_label,
        hjust = 0.5,
        vjust = ifelse(delta_value >= 0, -1.5, 2.5),
        color = color,
        size = 5,
        fontface = "bold"
      ) +
      labs(title = "Change in DRI (Pre vs. Post Deliberation)", x = "Deliberation Stage", y = "DRI") +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        ),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major.x = element_blank()
      )

    grid.arrange(
      plot_pre,
      plot_post,
      delta_plot,
      ncol = 2,
      layout_matrix = cbind(c(1, 3), c(2, 3))
    )

  })

  # Existing Render the plot
  output$case_plot <- renderPlot({
    reactive_case_plot()
  }, height = 700, width = 800)


  # --- LLM Data Generation Logic (FIXED: using withProgress and deferred saving) ---

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

    n_iterations <- input$n_iterations

    # Ensure n is valid
    if (n_iterations < 1 || n_iterations > 20) {
      showNotification("Error: 'n' must be between 1 and 20.", type = "error")
      return()
    }

    # Define the core metadata columns for validation
    METADATA_COLS_FOR_VALIDATION <- c(
      "created_at_utc",
      "provider",
      "model",
      "survey",
      "role_uid",
      "time_s",
      "est_cost_usd",
      "is_valid",
      "invalid_reason"
    )

    # Temporary storage for all n iterations (new data generated in this run)
    iterations_to_save <- initial_llm_data_structure

    # Use withProgress for robust progress indication
    withProgress(message = 'Generating LLM data...', value = 0, {
      # Loop n times, generating one iteration at a time
      for (i in 1:n_iterations) {
        # Update progress bar
        incProgress(1 / n_iterations,
                    detail = paste("Iteration", i, "of", n_iterations))

        tryCatch({
          # 1. Generate one response
          # input$model_id now comes from the dropdown, which correctly passes the "provider/model" string
          new_data <- deliberr::get_dri_llm_response(
            model_id = input$model_id,
            survey_name = input$survey_name,
            role_uid = input$role_uid
          )

          # Check for required metadata columns and if data was returned
          if (nrow(new_data) > 0 &&
              all(METADATA_COLS_FOR_VALIDATION %in% names(new_data))) {
            # 2. Append the new data to the reactive value (updates the table instantly)
            updated_data <- bind_rows(llm_results(), new_data)
            llm_results(updated_data)

            # 3. Collect for deferred saving
            iterations_to_save <- bind_rows(iterations_to_save, new_data)

          } else {
            showNotification(
              paste(
                "Iteration",
                i,
                "failed: LLM response did not contain expected data."
              ),
              type = "warning",
              duration = 5
            )
          }

        }, error = function(e) {
          showNotification(
            paste(
              "Iteration",
              i,
              "Failed (API Error):",
              conditionMessage(e)
            ),
            type = "error",
            duration = 5
          )
        })

        # Add a short sleep to yield control back to Shiny for UI update/other events
        Sys.sleep(0.5)
      }
    }) # End withProgress block


    # --- DEFERRED SAVING: Save all collected data only if any new data was successfully collected ---
    if (nrow(iterations_to_save) > 0) {
      # Determine if we need a header (i.e., if the persistent file does not exist yet)
      append_to_file <- file.exists("llm_data.csv")

      # Write all accumulated new data rows to the file at once
      # FIX: Use write.csv to ensure proper handling of commas/special characters in string fields
      write_csv(iterations_to_save, "llm_data.csv", append = append_to_file)

      showNotification(
        paste(
          "LLM Generation Complete! Added and saved",
          nrow(iterations_to_save),
          "iterations to llm_data.csv."
        ),
        type = "message"
      )
    } else {
      showNotification("LLM Generation Complete, but no valid data was generated or saved.",
                       type = "warning")
    }

  })

  # ** Event observer for the Refresh button **
  observeEvent(input$refresh_llm_data, {
    load_llm_data()
  })

  # Logic for Clearing LLM Data
  observeEvent(input$clear_llm_data, {
    llm_results(initial_llm_data_structure)

    # Clears in-app data only, preserving llm_data.csv
    showNotification(
      "In-app LLM data cleared. The 'llm_data.csv' file remains on disk for future loading.",
      type = "warning"
    )
  })

  # --- Logic for Uploading LLM Data ---
  observeEvent(input$upload_llm_file, {
    req(input$upload_llm_file)

    # Get the file path
    file_path <- input$upload_llm_file$datapath

    id <- showNotification("Uploading and parsing file...",
                           type = "default",
                           duration = NULL)

    tryCatch({
      # Read the CSV file. Assuming it contains all necessary columns from a previous export.
      # FIX: Use read.csv, which correctly handles fields quoted due to commas.
      uploaded_data <- read_csv(file_path, show_col_types = FALSE)

      # Append the uploaded data to the reactive value
      llm_results(uploaded_data)

      removeNotification(id = id)
      showNotification(paste(
        "Successfully uploaded and added",
        nrow(uploaded_data),
        "data points."
      ),
      type = "message")

    }, error = function(e) {
      removeNotification(id = id)
      showNotification(
        paste("Error reading/processing file:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })
  # --- End Logic for Uploading LLM Data ---

  # Render the accumulated LLM results table
  output$llm_results_table <- DT::renderDataTable({
    data_to_display <- llm_results()

    if (nrow(data_to_display) == 0 || is.null(data_to_display)) {
      return(DT::datatable(
        data.frame(Message = "No DRI data available to display."),
        options = list(dom = 't'),
        # Hide all controls, show table body only
        rownames = FALSE
      ))
    }

    # Define the columns to show in the table (metadata only)
    DISPLAY_METADATA_COLS <- c(
      "created_at_utc",
      "provider",
      "model",
      "survey",
      "role_uid",
      "time_s",
      "est_cost_usd",
      "is_valid",
      "invalid_reason"
    )

    # Filter the data only for display in the table
    data_for_datatable <- data_to_display %>%
      select(any_of(DISPLAY_METADATA_COLS))

    DT::datatable(
      data_for_datatable,
      # Use the filtered data for display
      options = list(
        # Set default sort on created_at_utc (newest first)
        order = list(0, 'desc'),
        pageLength = 10,
        scrollX = TRUE,
        # Allow horizontal scrolling for better viewing of all columns
        autoWidth = FALSE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      # Format numeric columns for readability
      DT::formatSignif(columns = c('time_s', 'est_cost_usd'),
                       digits = 4) %>%
      # Highlight invalid rows
      DT::formatStyle(
        columns = 'is_valid',
        target = 'row',
        backgroundColor = DT::styleEqual(FALSE, '#ffdddd') # Light red background for invalid rows
      )
  })

  # --- Logic for Downloading LLM Data ---
  output$download_llm_data <- downloadHandler(
    filename = function() {
      "llm_data.csv" # Default file name
    },
    content = function(file) {
      # Read the external file, which is the persistent record.

      # Read the external file, which is the persistent record.
      if (file.exists("llm_data.csv")) {
        file.copy("llm_data.csv", file)
      } else {
        # Fallback: if the file doesn't exist, generate a fresh one from reactive data.
        write_csv(llm_results(), file)
      }


    }
  )
  # --- End Logic for Downloading LLM Data ---

  # --- LLM Analysis Plotting & Summary Logic ---

  # Dynamic Filter Updates for LLM Analysis Tab
  observe({
    # Defensive check: ensure llm_results() is not empty before filtering
    if (nrow(llm_results()) == 0) {
      # When no data is present, set survey choices to empty and role/model choices to only "All"
      updateSelectInput(session,
                        "llm_survey_filter",
                        choices = character(0),
                        selected = NULL)
      updateSelectizeInput(session,
                           "llm_role_filter",
                           choices = "All",
                           selected = "All") # Updated to use updateSelectizeInput
      updateSelectInput(session,
                        "llm_model_filter",
                        choices = "All",
                        selected = "All")
      return()
    }

    # Filter for valid data before extracting unique choices
    current_data <- llm_results() %>% filter(is_valid == TRUE)

    # 1. Update Survey choices (REMOVING "All")
    survey_choices <- sort(unique(current_data$survey))
    selected_survey <- if (input$llm_survey_filter %in% survey_choices)
      input$llm_survey_filter
    else
      survey_choices[1]
    updateSelectInput(session,
                      "llm_survey_filter",
                      choices = survey_choices,
                      selected = selected_survey)

    # 2. Update Model choices (Including "All")
    # We should use the unique models that are actually in the results data, not the full list
    model_choices <- c("All", sort(unique(current_data$model)))
    updateSelectInput(session, "llm_model_filter", choices = model_choices)

    # 3. Update Role choices (Keeping "All") - using updateSelectizeInput
    role_choices <- c("All", sort(unique(current_data$role_uid)))
    updateSelectizeInput(
      session,
      "llm_role_filter",
      choices = role_choices,
      selected = input$llm_role_filter
    )
  })

  # Reactive expression to filter, calculate IC and DRI for plotting
  reactive_llm_plot_data <- reactive({
    data <- llm_results()

    # CRUCIAL: Require that a survey is selected before proceeding.
    req(input$llm_survey_filter)

    # 1. Filter only valid data
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Filter data for use in summary and plot
    data_filtered <- data

    # 2. Filter by survey (Guaranteed to have a single value due to req())
    data_filtered <- data_filtered %>% filter(survey == input$llm_survey_filter)

    # 3. Filter by model
    if (input$llm_model_filter != "All") {
      model_to_filter <- basename(input$llm_model_filter) # Extract model name from provider/model
      data_filtered <- data_filtered %>% filter(model == model_to_filter)
    }

    # 4. Filter by role (Updated to handle multi-select input: NULL, "All", or a vector of UIDs)
    selected_roles <- input$llm_role_filter
    if (!is.null(selected_roles) && !"All" %in% selected_roles) {
      data_filtered <- data_filtered %>% filter(role_uid %in% selected_roles)
    }

    # Defensive check: return NULL if data is filtered down to empty set
    if (nrow(data_filtered) == 0) {
      return(NULL)
    }

    # --- Separate valid data for IC/DRI calculation ---
    data_valid <- data_filtered %>% filter(is_valid == TRUE)

    # If no valid data, we can't calculate IC/DRI, but we can still show summary stats
    if (nrow(data_valid) == 0) {
      return(list(
        raw_data = data_filtered,
        ic = NULL,
        dri = NULL
      ))
    }

    # 5. Calculate IC and DRI on VALID data
    ic_llm <- deliberr::get_dri_ic(data_valid)
    dri <- deliberr::get_dri(ic_llm)

    list(raw_data = data_filtered,
         ic = ic_llm,
         dri = dri)
  })

  # Reactive expression to calculate summary statistics
  reactive_llm_summary <- reactive({
    plot_data_list <- reactive_llm_plot_data()

    # If filtering results in no data, return NULL or an empty structure
    if (is.null(plot_data_list)) {
      return(NULL)
    }

    data_filtered <- plot_data_list$raw_data

    total_iterations <- nrow(data_filtered)
    valid_iterations <- sum(data_filtered$is_valid)

    if (total_iterations == 0) {
      return(NULL)
    }

    # Calculate metrics
    total_cost <- sum(data_filtered$est_cost_usd, na.rm = TRUE)
    total_time <- sum(data_filtered$time_s, na.rm = TRUE)

    # Success rate calculation: Valid / Total (as requested in the prompt)
    success_rate <- valid_iterations / total_iterations

    # Create the summary data frame
    summary_df <- data.frame(
      Metric = c(
        "Total Iterations",
        "Valid Iterations",
        "Success Rate (Valid / Total)",
        "Total Cost (USD)",
        "Total Time (s)"
      ),
      Value = c(
        as.character(total_iterations),
        as.character(valid_iterations),
        paste0(round(success_rate * 100, 2), "%"),
        paste0("$", round(total_cost, 5)),
        round(total_time, 2)
      ),
      stringsAsFactors = FALSE
    )

    return(summary_df)
  })

  # Render the LLM Analysis Summary Table
  output$llm_summary_table <- DT::renderDataTable({
    summary_data <- reactive_llm_summary()

    if (is.null(summary_data)) {
      return(DT::datatable(
        data.frame(Message = "Filter selection resulted in no data."),
        options = list(dom = 't'),
        # Hide all controls, show table body only
        rownames = FALSE
      ))
    }

    DT::datatable(
      summary_data,
      options = list(
        dom = 't',
        # Only show the table body
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })

  # Render the LLM Analysis plot with dynamic, square sizing
  output$llm_plot <- renderPlot({
    plot_data <- reactive_llm_plot_data()

    if (is.null(plot_data) || is.null(plot_data$ic)) {
      # Return a placeholder plot or message if no valid data is available for DRI calculation
      return(
        ggplot() + annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No valid LLM data available for DRI calculation or plotting."
        ) + theme_void()
      )
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
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = error_message,
            color = "red",
            size = 5
          ) +
          theme_void() +
          labs(title = "Plot Generation Error")
      )
    })

    # FIX: Set height dynamically to match the width, maintaining a square aspect ratio.
  }, height = function()
    session$clientData$output_llm_plot_width)

  # Removed the observe block for loading llm_data.csv on startup.
}
