#' Get model ids
#'
#' Gets the provider and model names from openrouter. The \code{model_id} can
#' be recreated as \code{provider}/\code{model}
#'
#' @importFrom tidyr separate_wider_delim
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @returns A dataframe with columns \code{provider} and \code{model}
#' @export
#'
#' @examples
#'
#' get_model_ids()
#'
get_model_ids <- function() {
  # Check if model data is already cached (defined in get_dri_llm_data)
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
  model_ids <- .openrouter_cache$models %>%
    map(function(m) m$id) %>%
    enframe(name = NULL, value = "model_id") %>%
    tidyr::separate_wider_delim(.data$model_id, delim = "/", names = c("provider", "model"))


  return(model_ids)
}
