#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Here's how to deploy local packages:
# https://stackoverflow.com/questions/54536664/deploy-app-to-shinyapps-io-that-depends-on-custom-package-which-itself-has-depen
#

# Load necessary libraries
library(shiny)
library(deliberr)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)

# check for other necessary packages
if (!rlang::is_installed("gridExtra")) {
  install.packages("gridExtra")
}

if (!rlang::is_installed("DT")) {
  install.packages("DT")
}

library(gridExtra)
library(DT)


# --- Data Preparation for Model Dropdown (using deliberr::get_model_ids) ---
# NOTE: get_model_ids is called outside the server function for one-time initialization.
model_df <- deliberr::get_model_ids()

# a human-readable label for roles: name (uid)
ROLE_LABEL <- "%s (%s)"

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

  # 2. Reactive value to store all roles (initial deliberr roles + custom roles)
  roles_data <- reactiveVal(deliberr::roles)

  # 3. NEW: Reactive value to store uploaded human data (combined with deliberr default)
  human_data_combined <- reactiveVal(deliberr::human_data)

  # Initialize the model dropdown with the prepared choices
  observe({
    updateSelectInput(session,
                      "model_id",
                      choices = model_choices_list,
                      selected = default_model)

    # Initialize role creation dropdowns using current roles_data
    current_roles <- roles_data()
    updateSelectInput(session,
                      "new_role_type",
                      choices = sort(unique(current_roles$type)))

  }) # Run only once on startup


  # --- LLM Roles Management Logic (NEW) ---

  # Dynamic Updates for Role Creation and LLM Data Generation Tabs
  observe({
    current_roles <- roles_data() %>%
      bind_rows(tibble( # add default role
        role = "default"
      ))
    # %>%
    #   mutate(label = sprintf(ROLE_LABEL, role, uid)) # create label


    # 1. Update Role UID dropdown in the "LLM Data" tab
    # roles are values, uid is key
    role_choices <- current_roles %>%
      select(role, uid) %>%
      arrange(role) %>%
      deframe()
    updateSelectInput(session, "role_uid", choices = role_choices)


  })

  # Reactive expression to generate the template text preview
  output$role_template_preview <- renderUI({
    # Combine inputs for the template. Use placeholders if inputs are empty.
    # article <- ifelse(
    #   is.null(input$new_role_article) || input$new_role_article == "",
    #   "[article]",
    #   input$new_role_article
    # )
    role_name <- ifelse(
      is.null(input$new_role_name) || input$new_role_name == "",
      "[role]",
      input$new_role_name
    )
    description <- ifelse(
      is.null(input$new_role_description) || input$new_role_description == "",
      "[description]",
      input$new_role_description
    )

    # Construct the final HTML string
    template_text <- sprintf(
      '<span>Answer the following prompts as: <b>%s</b>, who <b>%s</b>.</span>',
      # article,
      role_name,
      description
    )

    HTML(template_text)
  })


  # Render the Roles Data Table
  output$roles_table <- DT::renderDataTable({
    data_to_display <- roles_data()

    DT::datatable(
      data_to_display,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20),
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(columns = 'uid', fontWeight = 'bold')
  })

  # Store pending role data for confirmation
  pending_role <- reactiveVal(NULL)

  # Event handler for showing confirmation modal
  observeEvent(input$add_custom_role, {
    # 1. Get and clean inputs
    uid <- trimws(input$new_role_uid)
    role_name <- trimws(input$new_role_name)
    description <- trimws(input$new_role_description)
    type_val <- "custom"
    # article_val <- input$new_role_article

    # 2. Validation Checks
    current_uids <- roles_data()$uid

    # UID Check
    if (nchar(uid) < 3 || nchar(uid) > 10 || !grepl("^[a-zA-Z0-9-]+$", uid)) {
      showNotification(
        "Error: UID length must be between 3-10 alphanumeric characters.",
        type = "error",
        duration = 5
      )
      return()
    }
    if (uid %in% current_uids) {
      showNotification(
        "Error: UID already exists. Please choose a unique ID.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Role Name Check (max 10 chars, letters/hyphens only)
    if (nchar(role_name) < 3 ||
        nchar(role_name) > 20 || !grepl("^[a-zA-Z -]+$", role_name)) {
      showNotification(
        "Error: Role name must be 3-20 characters, containing only letters, hyphens, and spaces.",
        type = "error",
        duration = 7
      )
      return()
    }

    # Description Check (max 100 words, letters/hyphens and spaces only)
    word_count <- length(unlist(strsplit(description, "\\s+")))
    if (word_count > 100) {
      showNotification(
        paste("Error: Description exceeds the 100-word limit. Current count:", word_count),
        type = "error",
        duration = 7
      )
      return()
    }

    # Check for invalid characters
    if (!grepl("^[a-zA-Z, -]+$", description)) {
      showNotification(
        "Error: Description contains invalid special characters. Only letters, spaces, hyphens, and commas are allowed.",
        type = "error",
        duration = 7
      )
      return()
    }

    # 3. Store the pending role and show confirmation modal
    pending_role(list(
      uid = uid,
      type = type_val,
      # article = article_val,
      role = role_name,
      description = description
    ))

    # Generate preview text for modal
    template_text <- sprintf(
      'Answer the following prompts as: <b>%s</b>, who <b>%s</b>.',
      # article_val,
      role_name,
      description
    )

    showModal(modalDialog(
      title = "Confirm New Role",
      HTML(paste0("<p><strong>Please review the role template:</strong></p>",
                  "<div style='padding: 15px; background-color: #f0f0f0; border-radius: 5px; margin: 10px 0;'>",
                  template_text,
                  "</div>",
                  "<p>Do you want to add this role?</p>")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_role", "Accept", class = "btn-success")
      ),
      easyClose = FALSE
    ))
  })

  # Event handler for confirming role addition
  observeEvent(input$confirm_add_role, {
    role_data <- pending_role()

    if (!is.null(role_data)) {
      # Create new row and append
      new_role <- data.frame(
        uid = role_data$uid,
        type = role_data$type,
        # article = role_data$article,
        role = role_data$role,
        description = role_data$description,
        stringsAsFactors = FALSE
      )

      # Append to reactive value
      updated_roles <- bind_rows(roles_data(), new_role)
      roles_data(updated_roles)

      # Save to custom_roles.csv
      tryCatch({
        custom_roles <- updated_roles %>% filter(type == "custom")
        write_csv(custom_roles, "custom_roles.csv")
      }, error = function(e) {
        showNotification(
          paste("Warning: Could not save custom roles:", conditionMessage(e)),
          type = "warning"
        )
      })

      # Success feedback and clear form
      showNotification(paste("Role", role_data$uid, "added successfully!"),
                       type = "message")

      # Clear form fields
      updateTextInput(session, "new_role_uid", value = "")
      updateTextInput(session, "new_role_name", value = "")
      updateTextAreaInput(session, "new_role_description", value = "")

      # Clear pending role
      pending_role(NULL)
    }

    removeModal()
  })

  # --- Logic for Downloading Custom Roles ---
  output$download_custom_roles <- downloadHandler(
    filename = function() {
      "custom_roles.csv"
    },
    content = function(file) {
      custom_roles <- roles_data() %>% filter(type == "custom")

      if (nrow(custom_roles) == 0) {
        # Create empty file with headers
        empty_df <- data.frame(
          uid = character(),
          type = character(),
          # article = character(),
          role = character(),
          description = character(),
          stringsAsFactors = FALSE
        )
        write_csv(empty_df, file)
      } else {
        write_csv(custom_roles, file)
      }
    }
  )

  # --- Logic for Uploading Custom Roles ---
  observeEvent(input$upload_custom_roles, {
    req(input$upload_custom_roles)

    file_path <- input$upload_custom_roles$datapath

    id <- showNotification("Uploading and parsing custom roles...",
                           type = "default",
                           duration = NULL)

    tryCatch({
      uploaded_roles <- read_csv(file_path, show_col_types = FALSE)

      # Validate structure
      required_cols <- c("uid", "type", "role", "description")
      if (!all(required_cols %in% names(uploaded_roles))) {
        removeNotification(id = id)
        showNotification(
          "Error: Uploaded file must contain columns: uid, type, role, description",
          type = "error",
          duration = 10
        )
        return()
      }

      # Ensure type is set to "custom"
      uploaded_roles <- uploaded_roles %>% mutate(type = "custom")

      # Check for UID conflicts with existing roles
      current_uids <- roles_data()$uid
      conflicting_uids <- uploaded_roles$uid[uploaded_roles$uid %in% current_uids]

      if (length(conflicting_uids) > 0) {
        removeNotification(id = id)
        showNotification(
          paste("Error: The following UIDs already exist:", paste(conflicting_uids, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }

      # Append uploaded roles
      updated_roles <- bind_rows(roles_data(), uploaded_roles)
      roles_data(updated_roles)

      # Save to custom_roles.csv
      custom_roles <- updated_roles %>% filter(type == "custom")
      write_csv(custom_roles, "custom_roles.csv")

      removeNotification(id = id)
      showNotification(
        paste("Successfully uploaded", nrow(uploaded_roles), "custom role(s)."),
        type = "message"
      )

    }, error = function(e) {
      removeNotification(id = id)
      showNotification(
        paste("Error reading/processing file:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })

  # Update the roles table rendering to highlight custom roles
  output$roles_table <- DT::renderDataTable({
    data_to_display <- roles_data()

    DT::datatable(
      data_to_display,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20),
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(columns = 'uid', fontWeight = 'bold') %>%
      DT::formatStyle(
        columns = 'type',
        target = 'row',
        backgroundColor = DT::styleEqual('custom', '#fff3cd') # Light yellow for custom roles
      )
  })

  # --- End LLM Roles Management Logic ---


  # --- NEW: Logic for Uploading Custom Human Data ---
  observeEvent(input$upload_human_data, {
    req(input$upload_human_data)

    file_path <- input$upload_human_data$datapath

    id <- showNotification("Uploading and parsing human data...",
                           type = "default",
                           duration = NULL)

    tryCatch({
      # Read the CSV file
      uploaded_data <- read_csv(file_path, show_col_types = FALSE)

      # Validate required columns
      required_cols <- c("survey", "case", "stage_id", "pnum",
                         paste0("C", 1:50), paste0("P", 1:10))
      if (!all(required_cols %in% names(uploaded_data))) {
        removeNotification(id = id)
        missing_cols <- setdiff(required_cols, names(uploaded_data))
        showNotification(
          paste("Error: Uploaded file must contain all required columns.",
                "Missing:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }

      # Check if case already exists
      uploaded_cases <- unique(uploaded_data$case)
      existing_cases <- unique(human_data_combined()$case)
      if (any(uploaded_cases %in% existing_cases)) {
        removeNotification(id = id)
        cases_overlap <- intersect(existing_cases, uploaded_cases)
        showNotification(
          paste("Error: Uploaded file contains existing cases.",
                "Existing:", paste(cases_overlap, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }

      # Append uploaded data to existing human_data
      updated_human_data <- bind_rows(human_data_combined(), uploaded_data)
      human_data_combined(updated_human_data)

      removeNotification(id = id)
      showNotification(
        paste("Successfully uploaded", nrow(uploaded_data), "data point(s)."),
        type = "message"
      )

    }, error = function(e) {
      removeNotification(id = id)
      showNotification(
        paste("Error reading/processing file:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })

  # --- End Logic for Uploading Custom Human Data ---


  # --- Existing Reactive Data Generation for Table ---
  reactive_case_dri_df <- reactive({
    # Initialize an empty list to store results for each case
    case_dri_list <- list()

    human_data_summary <- human_data_combined() %>%
      group_by(case, survey) %>%
      summarise(N = n()/2, .groups = "drop") # account for pre/post surveys

    # Loop through each unique case in the combined human data
    for (i in 1:nrow(human_data_summary)) {

      case <- human_data_summary[i,]$case
      survey <- human_data_summary[i,]$survey
      N <- human_data_summary[i,]$N

      dri_case <- deliberr::get_dri_case(
        case,
        method = input$method,
        adjusted = input$adjusted,
        alternative = input$alternative,
        data = human_data_combined()  # Pass the combined data
      )

      case_dri_list[[length(case_dri_list) + 1]] <- tibble(
        dri_case,
        survey,
        N
      )

    }

    # Combine the list of data frames into a single data frame
    bind_rows(case_dri_list)
  })

  # Existing Render the interactive data table
  output$case_table <- DT::renderDataTable({
    data_to_display <- reactive_case_dri_df() %>%
      select(case, survey, N, pre, post, delta, p_value, significance) %>%
      arrange(case, survey)

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
      DT::formatSignif(columns = c("pre", "post", "delta", "p_value"), digits = 2) %>%
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

  # --- NEW: Dynamic Update for Case Selector in Case Plot View ---
  observe({
    updated_cases <- unique(human_data_combined()$case)
    selected_case <- if (input$case_select %in% updated_cases)
      input$case_select
    else
      updated_cases[1]

    updateSelectInput(session, "case_select",
                      choices = updated_cases,
                      selected = selected_case)
  })

  # --- Existing Reactive Plot Generation ---
  reactive_case_plot <- reactive({
    # Ensure a case is selected
    req(input$case_select)

    # Filter data for the selected case using combined human data
    data_pre <- human_data_combined() %>% filter(case == input$case_select, stage_id == 1)
    # FIX: Corrected assignment operator (=) to equality operator (==)
    data_post <- human_data_combined() %>% filter(case == input$case_select, stage_id == 2)

    # Calculate indices
    ic_pre <- deliberr::get_dri_ic(data_pre)
    ic_post <- deliberr::get_dri_ic(data_post)

    # Calculate DRI
    dri_pre <- deliberr::get_dri(ic_pre)
    dri_post <- deliberr::get_dri(ic_post)

    # Generate plots
    plot_pre <- deliberr::plot_dri_ic(
      ic_pre,
      title = input$case_select,
      suffix = "pre",
      dri = dri_pre
    )
    plot_post <- deliberr::plot_dri_ic(
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
          # 0. Get role_info from uid
          role_info <- roles_data()[roles_data()$uid == input$role_uid, ]

          # 0. Get survey_info from name
          survey_info <- surveys_data()[surveys_data()$name == input$survey_name, ]

          # 1. Generate one response
          # input$model_id now comes from the dropdown, which correctly passes the "provider/model" string
          new_data <- deliberr::get_dri_llm_response(
            model_id = input$model_id,
            survey_info = survey_info,
            role_info = role_info
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
      # append_to_file <- file.exists("llm_data.csv")

      # Write all accumulated new data rows to the file at once
      # FIX: Use write.csv to ensure proper handling of commas/special characters in string fields
      # write_csv(iterations_to_save, "llm_data.csv", append = append_to_file)

      showNotification("LLM Generation Complete!", type = "message")

      showModal(modalDialog(
        title = "Reminder: Download LLM Data",
        HTML(paste0("<p><strong>Please remember to download your data, otherwise
                    it will be lost once the session ends.</strong></p>",
                    "<p>Do you want to download your LLM data now?</p>")),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("download_llm_data", "Download", class = "btn-success")
        ),
        easyClose = FALSE
      ))


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
      "In-app LLM data cleared.",
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

      removeModal()

    }
  )
  # --- End Logic for Downloading LLM Data ---

  observe({
    # Update survey dropdown in "LLM Data" tab
    updated_surveys <- sort(unique(surveys_data()$name))

    current_selection <- input$survey_name
    selected_survey <- if (!is.null(current_selection) && current_selection %in% updated_surveys)
      current_selection
    else if (length(updated_surveys) > 0)
      updated_surveys[1]
    else
      NULL

    updateSelectInput(session,
                      "survey_name",
                      choices = updated_surveys,
                      selected = selected_survey)

    # Update survey dropdown in "LLM Analysis" tab
    updateSelectInput(session,
                      "llm_survey_filter",
                      choices = updated_surveys,
                      selected = selected_survey)
  })

  # --- LLM Analysis Plotting & Summary Logic ---

  # Dynamic Filter Updates for LLM Analysis Tab
  observe({
    # Defensive check: ensure llm_results() is not empty before filtering
    if (nrow(llm_results()) == 0) {
      # When no data is present, set survey choices to empty and role/model choices to only "All"
      # updateSelectInput(session,
      #                   "llm_survey_filter",
      #                   choices = character(0),
      #                   selected = NULL)
      # The llm_role_filter is updated by the main roles_data observer.
      updateSelectizeInput(session,
                           "llm_model_filter",
                           choices = "all",
                           selected = "all")

      updateSelectizeInput(session,
                           "llm_role_filter",
                           choices = "all",
                           selected = "all")
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

    # 2. Update Model choices (Including "all")
    # We should use the unique models that are actually in the results data, not the full list
    model_choices <- c(sort(unique(current_data[current_data$survey == selected_survey, ]$model)))
    updateSelectizeInput(session, "llm_model_filter", choices = model_choices)

    # 3. Update Role choices
    role_choices <- current_data %>%
      filter(survey == selected_survey, model %in% model_choices) %>%
      group_by(role_uid) %>%
      left_join(roles_data() %>%
                  mutate(role_uid = uid), join_by(role_uid)) %>%
      select(role, role_uid) %>%
      unique() %>%
      mutate(role = case_when(
        is.na(role_uid) ~ "default",
        is.na(role) ~ paste(role_uid, "(missing)"),
        .default = role)) %>%
      deframe()

    updateSelectizeInput(session, "llm_role_filter", choices = role_choices)

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
    selected_models <- input$llm_model_filter
    if (!is.null(selected_models) && !"all" %in% selected_models) {
      data_filtered <- data_filtered %>% filter(model %in% selected_models)
    }

    # 4. Filter by role (Updated to handle multi-select input: NULL, "All", or a vector of UIDs)
    selected_roles <- input$llm_role_filter
    if (!is.null(selected_roles) && !"all" %in% selected_roles) {
      # check if the default role is in the selection
      if ("NA" %in% selected_roles) {
        data_filtered <- data_filtered %>%
          filter(role_uid %in% selected_roles | is.na(role_uid))
      } else {
        data_filtered <- data_filtered %>%
          filter(role_uid %in% selected_roles)
      }
    }



    # # --- START: New Human Data Inclusion Logic ---
    # human_append_type <- input$include_human_data
    #
    # if (human_append_type != "none") {
    #   # Determine target stage_id and the label for the plot
    #   stage_id <- if (human_append_type == "pre") 1 else 2
    #
    #   source_label <- if (stage_id == 1) "Human (Pre-Deliberation)" else "Human (Post-Deliberation)"
    #
    #   # Filter and standardize human data from the deliberr package
    #   human_subset_standardized <- deliberr::human_data %>%
    #     filter(
    #       survey == input$llm_survey_filter,
    #       stage_id == stage_id
    #     ) %>%
    #     mutate(
    #       is_valid = TRUE
    #     )
    #
    #   # Append the human data to the LLM data
    #   data_filtered <- bind_rows(data, human_subset_standardized) %>%
    #     select(-pnum)
    # }
    # # --- END: New Human Data Inclusion Logic ---



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
    average_time <- sum(data_filtered$time_s, na.rm = TRUE) / total_iterations
    cronbach_alpha <- deliberr::get_dri_alpha(data_filtered)

    # Success rate calculation: Valid / Total (as requested in the prompt)
    success_rate <- valid_iterations / total_iterations

    # Create the summary data frame
    summary_df <- data.frame(
      Metric = c(
        "Total iterations",
        "Valid iterations",
        "Success rate (valid / total)",
        "Total cost (USD)",
        "Average time per iteration (s)",
        "Cronbach alpha (considerations)",
        "Cronbach alpha (policies)"
      ),
      Value = c(
        as.character(total_iterations),
        as.character(valid_iterations),
        paste0(round(success_rate * 100, 2), "%"),
        paste0("$", round(total_cost, 5)),
        round(average_time, 2),
        round(cronbach_alpha$alpha_c, 3),
        round(cronbach_alpha$alpha_p, 3)
      ),
      stringsAsFactors = FALSE
    )

    return(summary_df)
  })



  # --- NEW: Reactive value to store uploaded surveys ---
  surveys_data <- reactiveVal(deliberr::surveys)

  # --- NEW: Observer for uploading custom surveys ---
  observeEvent(input$upload_survey_file, {
    req(input$upload_survey_file)

    file_path <- input$upload_survey_file$datapath

    id <- showNotification("Uploading and parsing survey...",
                           type = "default",
                           duration = NULL)

    tryCatch({
      # Read the CSV file
      uploaded_surveys <- read_csv(file_path, show_col_types = FALSE)

      # Validate required columns
      required_cols <- c("type", "order", "statement", "name", "scale_max", "q_method")
      if (!all(required_cols %in% names(uploaded_surveys))) {
        removeNotification(id = id)
        missing_cols <- setdiff(required_cols, names(uploaded_surveys))
        showNotification(
          paste("Error: Uploaded file must contain all required columns.",
                "Missing:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }

      # Check for duplicate survey names within the upload
      survey_names_upload <- unique(uploaded_surveys$name)
      if (length(survey_names_upload) != length(unique(uploaded_surveys$name))) {
        removeNotification(id = id)
        showNotification(
          "Error: Survey names must be unique within the uploaded file.",
          type = "error",
          duration = 10
        )
        return()
      }

      # Check if any survey names already exist in the combined data
      existing_surveys <- unique(surveys_data()$name)
      conflicting_surveys <- survey_names_upload[survey_names_upload %in% existing_surveys]

      if (length(conflicting_surveys) > 0) {
        removeNotification(id = id)
        showNotification(
          paste("Error: The following survey names already exist:", paste(conflicting_surveys, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }

      # Check if uploaded surveys contains consideration and policy statements
      for (name in unique(uploaded_surveys$name)) {

        survey_to_check <- uploaded_surveys %>%
          filter(name == !!name)

        statement_types <- survey_to_check %>%
          select(type) %>%
          deframe()

        ## check statement types
        if (!all("C" %in% statement_types, "P" %in% statement_types)) {
          removeNotification(id = id)
          showNotification(
            paste("Error: Survey", paste0("'", name,"'") ,"must contain statements of type C and P."),
            type = "error",
            duration = 10
          )
          return()
        }

        if (sum(statement_types == "C") < 2) {
          removeNotification(id = id)
          showNotification(
            paste("Error: Survey", paste0("'", name,"'") ,"must contain at least 2 consideration statements (type = 'C')."),
            type = "error",
            duration = 10
          )
          return()
        }

        if (sum(statement_types == "P") < 2) {
          removeNotification(id = id)
          showNotification(
            paste("Error: Survey", paste0("'", name,"'") ,"must contain at least 2 policy statements (type = 'P')."),
            type = "error",
            duration = 10
          )
          return()
        }

        # check statement orders
        o_c <- survey_to_check %>%
          filter(type == "C") %>%
          select(order) %>%
          deframe()

        if (!setequal(o_c, 1:length(o_c))) {
          removeNotification(id = id)
          showNotification(
            paste("Error: Survey", paste0("'", name,"'") ,"has considerations
                  statements with missing order:",
                  setdiff(1:length(o_c), o_c)),
            type = "error",
            duration = 10
          )
          return()
        }

        o_p <- survey_to_check %>%
          filter(type == "P") %>%
          select(order) %>%
          deframe()

        if (!setequal(o_p, 1:length(o_p))) {
          removeNotification(id = id)
          showNotification(
            paste("Error: Survey", paste0("'", name,"'") ,"has policy statements
                  with missing order:",
                  setdiff(1:length(o_p), o_p)),
            type = "error",
            duration = 10
          )
          return()
        }

      }

      # clean uploaded survey
      uploaded_surveys <- uploaded_surveys %>%
        filter(type == "C" | type == "P")

      uploaded_surveys[uploaded_surveys$type == "P", ]$scale_max <- NA
      uploaded_surveys[uploaded_surveys$type == "P", ]$q_method <- NA

      # Append uploaded surveys to existing surveys
      updated_surveys <- bind_rows(
        surveys_data(),
        uploaded_surveys
      )
      surveys_data(updated_surveys)

      removeNotification(id = id)
      showNotification(
        paste("Successfully uploaded", length(unique(uploaded_surveys$name)), "survey(s) with",
              nrow(uploaded_surveys), "question(s) total."),
        type = "message"
      )

    }, error = function(e) {
      removeNotification(id = id)
      showNotification(
        paste("Error reading/processing file:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })

  # --- NEW: Render surveys table with highlighting ---
  output$surveys_table <- DT::renderDataTable({
    data_to_display <- surveys_data() %>%
      select(type, order, statement, name, scale_max, q_method)

    DT::datatable(
      data_to_display,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50),
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '300px', targets = 2) # Make 'statement' column wider
        )
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(
        columns = 'name',
        target = 'row'
      )
  })


  # --- Human+LLM Analysis Tab Logic (NEW) ---

  # Dynamic updates for case selector in Human+LLM Analysis
  observe({
    updated_cases <- human_data_combined() %>%
      mutate(label = paste0(case, " (", survey, ")")) %>%
      select(label, case) %>%
      arrange(label) %>%
      unique() %>%
      deframe()
    # (nrow(llm_results()) > 0)
    selected_case <- if (length(updated_cases) > 0 && input$hlm_case_select %in% updated_cases)
      input$hlm_case_select
    else if (length(updated_cases) > 0)
      updated_cases[1]
    else
      NULL

    updateSelectInput(session, "hlm_case_select",
                      choices = updated_cases,
                      selected = selected_case)
  })

  # # Display survey for selected case
  # output$hlm_survey_display <- renderText({
  #   req(input$hlm_case_select)
  #
  #   survey_value <- human_data_combined() %>%
  #     filter(case == input$hlm_case_select) %>%
  #     pull(survey) %>%
  #     unique() %>%
  #     first()
  #
  #   if (is.na(survey_value)) {
  #     return("No survey found")
  #   }
  #
  #   survey_value
  # })

  # Dynamic updates for model and role selectors
  observe({
    req(input$hlm_case_select)

    # Get survey for selected case
    survey_value <- human_data_combined() %>%
      filter(case == input$hlm_case_select) %>%
      pull(survey) %>%
      unique() %>%
      first()

    # Ensure LLM results exist
    if (nrow(llm_results()) == 0) {
      updateSelectizeInput(session, "hlm_model_select", choices = character(0))
      updateSelectizeInput(session, "hlm_role_select", choices = character(0))
      return()
    }

    # Get unique models from llm_results that match the survey
    available_models <- llm_results() %>%
      filter(survey == survey_value) %>%
      pull(model) %>%
      unique() %>%
      sort()

    # FIXME
    selected_models <- if (length(available_models) > 0)
      available_models else input$hlm_model_select

    updateSelectInput(session, "hlm_model_select",
                      choices = available_models,
                      selected = selected_models)

    # Get unique roles for selected model
    if (!is.null(selected_models) && length(selected_models) > 0) {
      available_roles <- llm_results() %>%
        filter(survey == survey_value, model %in% selected_models) %>%
        pull(role_uid) %>%
        unique()

      # Create mapping from uid to role name
      role_mapping <- roles_data() %>%
        filter(uid %in% available_roles) %>%
        select(role, uid) %>%
        deframe()

      # Add any role_uid values not found in roles_data
      missing_roles <- setdiff(available_roles, role_mapping)
      for (uid in missing_roles) {
        if (is.na(uid)) role_mapping["default"] <- uid
        else role_mapping[paste(uid, "(missing)")] <- uid
      }

      updateSelectizeInput(session, "hlm_role_select",
                           choices = role_mapping)
    }
  })

  # Display human and LLM data counts
  output$hlm_human_count <- renderText({
    req(input$hlm_case_select, input$hlm_stage_select)

    human_count <- human_data_combined() %>%
      filter(case == input$hlm_case_select, stage_id == as.numeric(input$hlm_stage_select)) %>%
      nrow()

    paste0("Human data rows: ", human_count)
  })

  output$hlm_llm_count <- renderText({
    req(input$hlm_case_select, input$hlm_model_select, input$hlm_role_select)

    # Get survey for selected case
    survey_value <- human_data_combined() %>%
      filter(case == input$hlm_case_select) %>%
      pull(survey) %>%
      unique() %>%
      first()

    if (is.null(input$hlm_role_select) || length(input$hlm_role_select) == 0) {
      return("LLM data rows: 0")
    }

    llm_count <- if ("NA" %in% input$hlm_role_select) llm_results() %>%
      filter(
        model %in% input$hlm_model_select,
        survey == survey_value,
        role_uid %in% input$hlm_role_select | is.na(role_uid),
        is_valid == TRUE
      ) %>%
      nrow() else llm_results() %>%
      filter(
        model %in% input$hlm_model_select,
        survey == survey_value,
        role_uid %in% input$hlm_role_select,
        is_valid == TRUE
      ) %>%
      nrow()

    paste0("LLM data rows: ", llm_count)
  })

  # Reactive expression to generate human-only plot
  reactive_hlm_plot <- reactive({
    req(input$hlm_case_select, input$hlm_stage_select, input$hlm_model_select, input$hlm_role_select)

    # Ensure role selection is not empty
    if (is.null(input$hlm_role_select) || length(input$hlm_role_select) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "Please select at least one role."
          ) +
          theme_void()
      )
    }

    tryCatch({
      # 1. Get human data for selected case and stage
      human_subset <- human_data_combined() %>%
        filter(case == input$hlm_case_select, stage_id == as.numeric(input$hlm_stage_select)) %>%
        mutate(is_valid = TRUE)

      survey_value <- human_data_combined() %>%
        filter(case == input$hlm_case_select) %>%
        pull(survey) %>%
        unique() %>%
        first()

      # 2. Get LLM data for selected model and role(s)
      llm_subset <- if ("NA" %in% input$hlm_role_select) llm_results() %>%
        filter(
          model %in% input$hlm_model_select,
          survey == survey_value,
          role_uid %in% input$hlm_role_select | is.na(role_uid),
          is_valid == TRUE
        ) else llm_results() %>%
        filter(
          model %in% input$hlm_model_select,
          survey == survey_value,
          role_uid %in% input$hlm_role_select,
          is_valid == TRUE
        )

      # 3. Check if there's any human data
      if (nrow(human_subset) == 0) {
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "No human data found for selected case and stage."
            ) +
            theme_void()
        )
      }

      # 4. Calculate IC and DRI for human-only data
      ic_human <- deliberr::get_dri_ic(human_subset)
      dri_human <- deliberr::get_dri(ic_human)

      # 5. Generate human-only plot
      plot_human <- deliberr::plot_dri_ic(
        ic_human,
        title = input$hlm_plot_title_human,
        suffix = "human",
        dri = dri_human
      )

      # FIXME
      human_subset$pnum <- as.integer(rownames(human_subset))
      llm_subset$pnum <- -as.integer(rownames(llm_subset))

      # 6. Combine data for second plot
      common_cols <- intersect(names(human_subset), names(llm_subset))

      combined_data <- bind_rows(
        human_subset %>% select(all_of(common_cols)),
        llm_subset %>% select(all_of(common_cols))
      )

      # 7. Calculate IC and DRI for combined data
      if (nrow(combined_data) == 0) {
        plot_combined <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No valid LLM data found for selected filters."
          ) +
          theme_void()
      } else {
        ic_combined <- deliberr::get_dri_ic(combined_data)
        dri_combined <- deliberr::get_dri(ic_combined)

        plot_combined <- deliberr::plot_dri_ic(
          ic_combined,
          title = input$hlm_plot_title_combined,
          suffix = "combined",
          dri = dri_combined
        )
      }

      # 8. Arrange both plots side by side
      grid.arrange(
        plot_human,
        plot_combined,
        ncol = 2
      )

    }, error = function(e) {
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = paste("Plotting Failed (R Error):", conditionMessage(e)),
          color = "red",
          size = 4
        ) +
        theme_void() +
        labs(title = "Plot Generation Error")
    })
  })

  # Render the comparison plot with dynamic sizing
  output$hlm_comparison_plot <- renderPlot({
    reactive_hlm_plot()
  }, height = function() {
    session$clientData$output_hlm_comparison_plot_width / 2
  }, width = function() {
    session$clientData$output_hlm_comparison_plot_width
  })

  # --- End Human+LLM Analysis Tab Logic ---


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
}
