#' Send a request to a model on OpenRouter.ai
#'
#' This function sends a system prompt and a user prompt to a specified
#' language model through the OpenRouter.ai API and returns the text response.
#' It can also manage conversation history.
#'
#' @param user_prompt A string containing the prompt or question for the model.
#' @param model_id A string specifying the model to use (e.g., "google/gemini-flash-1.5").
#'        You can find model names on the OpenRouter.ai website.
#' @param system_prompt A string defining the role or behavior of the model. This is
#'        only used for the first message in a conversation (when 'context' is NULL).
#' @param context A list representing the conversation history. If provided, the
#'        'system_prompt' is ignored, as the context is assumed to contain the full
#'        history. Defaults to NULL for a new conversation.
#' @param temperature A numeric value between 0 and 2 that controls the randomness
#'        of the model's output. Higher values mean more creative responses.
#' @param context_length An integer specifying the maximum number of tokens
#'        (words and punctuation) in the response. This is also known as `max_tokens`.
#' @param reasoning A logical toggle (TRUE/FALSE). When set to TRUE, it hints to
#'        the model that it should "think" or use reasoning steps. The exact
#'        behavior is model-dependent. It works by setting `tool_choice` to `"auto"`.
#' @param api_key A string containing your OpenRouter.ai API key. It is strongly
#'        recommended to use the default, which retrieves the key from an
#'        environment variable named `OPENROUTER_API_KEY`.
#'
#' @return A list containing three elements: 'response', 'context', and 'cost'.
#'         'cost' is itself a list containing 'prompt_cost', 'completion_cost',
#'         and 'total_cost' in USD.
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure to set your API key first
#' # Sys.setenv(OPENROUTER_API_KEY = "your_api_key_here")
#'
#' # First turn of the conversation
#' first_turn <- get_llm_response(
#'   user_prompt = "What are the three main benefits of using R for data analysis?",
#'   model_id = "x-ai/grok-3-mini",
#'   system_prompt = "You are a helpful assistant who provides concise answers."
#' )
#' cat("--- Initial Response ---\n")
#' cat(first_turn$response)
#' cat(paste0("\n--- Total Cost: $", format(first_turn$cost$total_cost, scientific = FALSE), " ---\n"))
#'
#' # Follow-up question using the context from the first turn
#' second_turn <- get_llm_response(
#'   user_prompt = "Can you elaborate on the second benefit you mentioned?",
#'   model_id = "x-ai/grok-3-mini",
#'   context = first_turn$context
#' )
#' cat("\n\n--- Follow-up Response ---\n")
#' cat(second_turn$response)
#' cat(paste0("\n--- Total Cost: $", format(second_turn$cost$total_cost, scientific = FALSE), " ---\n"))
#' }
get_llm_response <- function(user_prompt,
                             model_id = "x-ai/grok-3-mini",
                             system_prompt = "You are a helpful assistant.",
                             context = NULL,
                             temperature = 0,
                             # max_tokens = 2048,
                             enable_reasoning = FALSE,
                             reasoning_effort = "low",
                             api_key = Sys.getenv("OPENROUTER_API_KEY")) {

  # Validate API key
  if (api_key == "") {
    stop("API key is not found. Please set the OPENROUTER_API_KEY environment variable or pass it directly to the function.")
  }

  # API endpoint
  url <- "https://openrouter.ai/api/v1/chat/completions"

  # Headers
  headers <- c(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  # Construct the message list
  messages <- list()
  if (is.null(context)) {
    # Start a new conversation with the system prompt if no context is provided
    messages <- list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  } else {
    # Continue an existing conversation by appending the new user prompt
    messages <- c(context, list(list(role = "user", content = user_prompt)))
  }

  # Set up reasoning parameters
  reasoning <- c(
    "enabled" = enable_reasoning,
    "effort" = reasoning_effort
  )

  # Construct the body of the request
  body <- list(
    model = model_id,
    messages = messages,
    temperature = temperature
    # max_tokens = max_tokens,
    # reasoning = reasoning
  )

  # Add reasoning parameter if requested
  # if (reasoning) {
  #   body$tool_choice <- "auto"
  # }

  # Make the POST request
  response <- httr::POST(
    url = url,
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )

  # Check for errors in the response
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(
      sprintf(
        "API request failed with status %d\nError message: %s",
        httr::status_code(response),
        error_content
      )
    )
  }

  # Parse the response content
  parsed_content <- httr::content(response, "parsed")

  # Get usage costs (in tokens)
  usage <- parsed_content$usage

  # Extract the text from the response
  text_response <- parsed_content$choices[[1]]$message$content

  # Create the assistant's message to be added to the context
  assistant_message <- parsed_content$choices[[1]]$message

  # Update the context for the next turn
  updated_context <- c(messages, list(assistant_message))

  # Return a list containing the response, updated context, and cost details
  return(list(
    response = text_response,
    context = updated_context,
    usage = usage
  ))
}
