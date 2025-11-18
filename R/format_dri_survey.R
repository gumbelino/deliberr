#' Formats surveys for further processing.
#'
#' @param survey_info survey information needed to format dri survey.
#'
#' @returns A list of survey info.
#' @export
#'
#' @examples
#' \dontrun{
#' dri_survey <- format_dri_survey(deliberr::surveys %>% filter(name == "acp"))
#' }
format_dri_survey <- function(survey_info = list(
  type = NA_character_,
  order = NA_integer_,
  statement = NA_character_,
  name = NA_character_,
  scale_max = NA_integer_,
  q_method = NA,
)) {

  name <- survey_info$name[1]
  scale_max <- survey_info$scale_max[1]
  q_method <- survey_info$q_method[1]

  c_df <- survey_info %>%
    filter(name == !!name, type == "C") %>%
    select(order, statement) %>%
    arrange(order)

  p_df <- survey_info %>%
    filter(name == !!name, type == "P") %>%
    select(order, statement) %>%
    arrange(order)

  list(
    name = name,
    considerations = c_df,
    policies = p_df,
    scale_max = scale_max,
    q_method = q_method
  )

}
