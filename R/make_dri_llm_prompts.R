#' Make DRI LLM prompts
#'
#' Creates the prompts for generating LLM DRI data.
#'
#' @param survey_info A list generated using get_dri_survey_info().
#' @param role_uid The unique identifier of a role (optional).
#'
#' @returns A list of lists with four variables: \code{system},
#' \code{considerations}, \code{policies}, and \code{reason}.
#' @export
#'
#' @examples
#' survey_info <- get_dri_survey_info("ccps")
#' make_dri_llm_prompts(survey_info[[1]], "csk")
make_dri_llm_prompts <- function(survey_info, role_uid = NA_character_) {

  ## get prompt templates
  prompt_c_template <- prompts[prompts$type == "considerations",]$prompt
  prompt_p_template <- prompts[prompts$type == "policies",]$prompt
  prompt_r <- prompts[prompts$type == "reason",]$prompt
  prompt_q <- prompts[prompts$type == "q_method",]$prompt
  prompt_s_template <- prompts[prompts$type == "system",]$prompt

  ## extract survey info
  scale_max <- survey_info$scale_max
  q_method <- if (survey_info$q_method) prompt_q else ""

  c_df <- survey_info$considerations
  p_df <- survey_info$policies

  n_c <- nrow(c_df)
  n_p <- nrow(p_df)

  ## get statements
  c_statements <- paste(paste0(c_df$order, ". ", c_df$statement), collapse = "\n")
  p_statements <- paste(paste0(p_df$order, ". ", p_df$statement), collapse = "\n")

  ## make prompts
  prompt_c <-
    paste0(sprintf(prompt_c_template, n_c, scale_max, scale_max, q_method, n_c, n_c),
           c_statements)

  prompt_p <-
    paste0(sprintf(prompt_p_template, n_p, n_p, n_p, n_p, n_p),
           p_statements)

  ## make system prompt
  if (is.na(role_uid)) {
    prompt_s <- "You are a helpful assistant." # generic role
  } else {
    # get roles
    s_article <- roles[roles$uid == role_uid,]$article
    s_role <- roles[roles$uid == role_uid,]$role
    s_description <- roles[roles$uid == role_uid,]$description

    # build prompt
    prompt_s <- sprintf(prompt_s_template, s_article, s_role, s_description)
  }

  return(list(
    system = prompt_s,
    considerations = prompt_c,
    policies = prompt_p,
    reason = prompt_r
  ))

}
