#' Make DRI LLM prompts
#'
#' Creates the prompts for generating LLM DRI data.
#'
#' @param dri_survey A list generated using format_dri_survey(...).
#' @param role_info Information about a specific role.
#'
#' @returns A list of lists with four variables: \code{system},
#' \code{considerations}, \code{policies}, and \code{reason}.
#' @export
#'
#' @examples
#' dri_survey <- format_dri_survey(deliberr::surveys[surveys$name == "ccps",])
#' role_info <- list(
#'   uid = "sur",
#'   role = "surfer",
#'   description = "likes the ocean"
#' )
#' make_dri_llm_prompts(dri_survey, role_info)
make_dri_llm_prompts <- function(dri_survey,
                                 role_info = list(
                                   uid = NA_character_,
                                   role = NA_character_,
                                   description = NA_character_
                                 )) {

  # make explicit where prompts is imported from
  prompts <- deliberr::prompts

  ## get prompt templates
  prompt_c_template <- prompts[prompts$type == "considerations",]$prompt
  prompt_p_template <- prompts[prompts$type == "policies",]$prompt
  prompt_r <- prompts[prompts$type == "reason",]$prompt
  prompt_q <- prompts[prompts$type == "q_method",]$prompt
  prompt_s_template <- prompts[prompts$type == "system",]$prompt

  ## extract survey info
  scale_max <- dri_survey$scale_max
  q_method <- if (dri_survey$q_method) prompt_q else ""

  # ensure statements are shuffled
  c_df <- dri_survey$considerations
  p_df <- dri_survey$policies

  n_c <- nrow(c_df)
  n_p <- nrow(p_df)

  ## get statements
  c_statements <- paste(paste0(c_df$shuffle, ". ", c_df$statement), collapse = "\n")
  p_statements <- paste(paste0(p_df$shuffle, ". ", p_df$statement), collapse = "\n")

  ## make prompts
  prompt_c <-
    paste0(sprintf(prompt_c_template, n_c, scale_max, scale_max, q_method, n_c, n_c),
           c_statements)

  prompt_p <-
    paste0(sprintf(prompt_p_template, n_p, n_p, n_p, n_p, n_p),
           p_statements)

  ## make system prompt
  if (length(role_info$uid) == 0 || is.na(role_info$uid)) {
    prompt_s <- NA_character_
  } else {
    # get roles
    s_role <- role_info$role
    s_description <- role_info$description

    # build prompt
    prompt_s <- sprintf(prompt_s_template, s_role, s_description)
  }

  return(list(
    system = prompt_s,
    considerations = prompt_c,
    policies = prompt_p,
    reason = prompt_r
  ))

}
