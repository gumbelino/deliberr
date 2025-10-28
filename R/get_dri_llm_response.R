#' Get DRI LLM response
#'
#' Uses openrouter.ai to generate LLM responses to DRI survey questions.
#'
#' @param model_id The model_id from openrounter.ai
#' @param survey_name The name of the survey
#' @param role_uid The unique identifier of a role (optional)
#' @param n The number of responses requested (dafault = 1)
#'
#' @returns A data frame with one row of data.
#' @export get_dri_llm_response
#'
#' @import tibble
#' @importFrom uuid UUIDgenerate
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#'
#' @examples
#'
#' model_id <- "google/gemini-2.5-flash-lite"
#' survey_name <- "ccps"
#' \dontrun{
#' llm_data <- get_dri_llm_response(model_id, survey_name, "eco", 5)
#' }
get_dri_llm_response <- function(model_id,
                                 survey_name,
                                 api_key = Sys.getenv("OPENROUTER_API_KEY"),
                                 role_info = list(
                                   uid = NA_character_,
                                   role = NA_character_,
                                   article = NA_character_,
                                   description = NA_character_
                                 ),
                                 n = 1) {

  # set time to UTC for consistent logging
  Sys.setenv(TZ='UTC')

  survey_names <- sort(unique(surveys$name))

  if (!survey_name %in% survey_names) {
    stop("Invalid survey name: ",
         survey_name,
         "\nValid names include:\n",
         paste(paste0(1:length(survey_names), ". ", sort(survey_names)), collapse = "\n"))
  }

  split_parts <- strsplit(model_id, "/")[[1]]

  ### GET SURVEY INFO
  survey_infos <- get_dri_survey_info(survey_name)
  survey_info <- survey_infos[[1]]

  llm_data <- list()

  for (i in 1:n) {

    # CREATE META DATA to be attached to request logs
    meta <- tibble(
      uuid = UUIDgenerate(),
      created_at_utc = Sys.time(),
      provider = split_parts[1],
      model = split_parts[2],
      survey = survey_name,
      role_uid = role_info$uid,
    )


    ### MAKE PROMPT and shuffle statements
    shuffled_info <- .shuffle_statements(survey_info)

    prompts <- make_dri_llm_prompts(shuffled_info, role_info)


    ### GET LLM RESPONSES
    est_cost_usd <- 0

    res <- get_llm_response(prompts$considerations, model_id = model_id, system_prompt = prompts$system)
    response_c <- res$response
    est_cost_usd <- est_cost_usd + .calculate_cost(res$usage, model_id)
    log <- .log_request(meta, "considerations", prompts$considerations, res)

    res <- get_llm_response(prompts$policies, model_id = model_id, context = res$context)
    response_p <- res$response
    est_cost_usd <- est_cost_usd + .calculate_cost(res$usage, model_id)
    log <- bind_rows(log, .log_request(meta, "policies", prompts$policies, res))

    res <- get_llm_response(prompts$reason, model_id = model_id, context = res$context)
    response_r <- res$response
    est_cost_usd <- est_cost_usd + .calculate_cost(res$usage, model_id)
    log <- bind_rows(log, .log_request(meta, "reason", prompts$reason, res))

    ## PARSE RESPONSES
    considerations <- .parse_llm_response(response_c, 50, "C", shuffled_info)
    policies <- .parse_llm_response(response_p, 10, "P", shuffled_info)
    reason <- .parse_llm_response(response_r, 1, "R")

    validity <- .is_valid_response(considerations, policies, survey_info)

    end_time <- Sys.time()
    time_s <- as.numeric(difftime(end_time, meta$created_at_utc, units = "secs"))

    llm_data[[length(llm_data)+1]] <- tibble(
      meta,
      time_s,
      est_cost_usd,
      validity,
      considerations,
      policies,
      reason
    )

    ## append log to
    if (file.exists("request_log.csv"))
      write_csv(log, "request_log.csv", append = TRUE)
    else
      write_csv(log, "request_log.csv")

    # log result
    progress <- if (n > 1) paste0("[",i,"/",n,"] ") else ""
    status <- if (validity$is_valid) "SUCCESS: " else "ERROR! "
    message(progress,
            status,"LLM response generated in ",
            round(time_s, 1), "s")

  }

  llm_data <- bind_rows(llm_data)

  return(llm_data)

}


# --- Internal Caching for Model Prices ---
.openrouter_cache <- new.env(parent = emptyenv())

.get_model_pricing <- function(model_id) {
  # Check if model data is already cached
  if (is.null(.openrouter_cache$models)) {
    # message("Fetching model pricing information from OpenRouter...")
    response <- httr::GET("https://openrouter.ai/api/v1/models")
    if (httr::status_code(response) == 200) {
      .openrouter_cache$models <- httr::content(response, "parsed")$data
    } else {
      stop("Failed to fetch model pricing information.")
    }
  }

  # Find the specific model in the cached data
  model_info <- Filter(function(m) m$id == model_id, .openrouter_cache$models)

  if (length(model_info) == 0) {
    warning("Could not find pricing information for model: ", model_id)
    return(list(prompt = 0, completion = 0))
  }

  pricing <- model_info[[1]]$pricing

  return(list(
    prompt = as.numeric(pricing$prompt),
    completion = as.numeric(pricing$completion)
  ))
}

.calculate_cost <- function(usage, model_id) {

  prompt_tokens <- usage$prompt_tokens
  completion_tokens <- usage$completion_tokens

  pricing <- .get_model_pricing(model_id)

  prompt_cost <- prompt_tokens * pricing$prompt
  completion_cost <- completion_tokens * pricing$completion
  total_cost <- prompt_cost + completion_cost

  return(total_cost)
}

.shuffle_statements <- function(survey_info) {
  survey_info$considerations <- survey_info$considerations %>%
    mutate(shuffle = sample(order)) %>%
    arrange(shuffle)
  survey_info$policies <- survey_info$policies %>%
    mutate(shuffle = sample(order)) %>%
    arrange(shuffle)
  survey_info
}

.parse_llm_response <- function(response, max_cols=c(50, 10, 1), col_prefix=c("C", "P", "R"), shuffled_info=NULL) {

  # check for reasoning case
  if (col_prefix == "R") {
    return(tibble(
      R = gsub("[\r\n]+$", "", response) ## remove trailing newlines
    ))
  }

  lines <- unlist(strsplit(trimws(response), "\n"))
  data_matrix <- do.call(rbind, strsplit(lines, "\\. "))

  # retrieve shuffled order
  if (col_prefix == "C")
    order <- shuffled_info$considerations$order
  else if (col_prefix == "P")
    order <- shuffled_info$policies$order
  else
    order <- as.numeric(data_matrix[, 1])

  df <- data.frame(
    order = order,
    value = as.numeric(data_matrix[, 2])
  )

  # unshuffle statements
  df <- df %>%
    arrange(order) %>%
    mutate(sid = paste0(col_prefix, order)) %>%
    select(sid, value)

  df <- pivot_wider(df, names_from = "sid", values_from = "value")

  num_cols <- ncol(df)

  if (is.na(max_cols) || num_cols >= max_cols) {
    return(df)
  }

  # Calculate the number of columns to add
  cols_to_add <- max_cols - num_cols

  # Create a new data frame with the columns to be added
  new_cols <- data.frame(matrix(NA, nrow = nrow(df), ncol = cols_to_add))

  # Generate names for the new columns
  names(new_cols) <- paste0(col_prefix, (num_cols + 1):max_cols)

  # Combine the original data frame with the new columns
  # The use of `cbind` ensures the new columns are appended
  combined_df <- cbind(df, new_cols)

  return(combined_df)
}

.log_request <- function(meta, type, prompt, res) {

  tibble(
    meta,
    type,
    prompt_tokens = res$usage$prompt_tokens,
    completion_tokens = res$usage$completion_tokens,
    prompt,
    response = res$response,
  )

}

.is_valid_response <- function(considerations, policies, survey_info) {

  # Extract relevant data from survey_info
  c_ranks <- considerations %>% select(matches("^C\\d+$") & where(~!all(is.na(.))))
  p_ranks <- policies %>% select(matches("^P\\d+$") & where(~!all(is.na(.))))
  scale_max <- survey_info$scale_max
  q_method <- survey_info$q_method

  validity <- tibble(
    is_valid = TRUE,
    invalid_reason = NA_character_
  )

  # Check if data is valid (length mismatch)
  if (ncol(c_ranks) != nrow(survey_info$considerations)) {
    message(paste("ERROR: Considerations length mismatch (", ncol(c_ranks), "/", nrow(survey_info$considerations), ")."))
    validity$is_valid = FALSE; validity$invalid_reason = "c_length_mismatch"
    return(validity)
  }

  if (ncol(p_ranks) != nrow(survey_info$policies)) {
    message(paste("ERROR: Policies length mismatch (", ncol(p_ranks), "/", nrow(survey_info$policies), ")."))
    validity$is_valid = FALSE; validity$invalid_reason = "p_length_mismatch"
    return(validity)
  }

  # Check if c_ranks contains invalid values
  if (any(c_ranks > scale_max | c_ranks < 1)) {
    message("ERROR: Consideration ranks contain invalid values.")
    validity$is_valid = FALSE; validity$invalid_reason = "c_invalid_values"
    return(validity)
  }

  # Check if p_ranks contains invalid values
  if (any(p_ranks > ncol(p_ranks) | p_ranks < 1)) {
    message("ERROR: Policy ranks contain invalid values.")
    validity$is_valid = FALSE; validity$invalid_reason = "p_invalid_values"
    return(validity)
  }

  # Check for duplicate values in p_ranks
  if (ncol(p_ranks) != length(unique(unlist(p_ranks)))) {
    message("ERROR: Policy ranks contains duplicate values.")
    validity$is_valid = FALSE; validity$invalid_reason = "p_duplicate_ranks"
    return(validity)
  }

  # Check for quasi-normality (assuming a quasi_normality_check function exists in R)
  if (q_method && !.quasi_normality_check(c_ranks)) {
    message("ERROR: Considerations do not follow a Fixed Quasi-Normal Distribution.")
    validity$is_valid = FALSE; validity$invalid_reason = "c_not_q_method"
    return(validity)
  }

  # Check if all considerations are the same value
  if (length(unique(unlist(c_ranks))) == 1) {
    message("ERROR: All considerations have the same rating.")
    validity$is_valid = FALSE; validity$invalid_reason = "c_all_equal"
    return(validity)
  }

  return(validity)
}


# FIXME: make check more robust
.quasi_normality_check <- function(ratings) {

  mean_val <- mean(ratings)
  median_val <- median(ratings)
  iqr_val <- IQR(ratings)

  # Define rough criteria (adjust as needed)
  is_quasi_normal <- abs(mean_val - median_val) < 10 && iqr_val < 30

  return(is_quasi_normal)
}

