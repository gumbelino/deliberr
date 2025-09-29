#' Get survey info
#'
#' @param survey_name the name of the survey (optional).
#'
#' @returns A list of survey info.
#' @export
#'
#' @examples
#' survey_info <- get_dri_survey_info("acp")
#' survey_info[[1]]$name
#' survey_info[[1]]$considerations
#' length(survey_info[[1]]$considerations$statement)
#' survey_info[[1]]$policies
#' length(survey_info[[1]]$policies$statement)
#' survey_info[[1]]$scale_max
#' survey_info[[1]]$q_method
get_dri_survey_info <- function(survey_name = NA_character_) {

  survey_names <- sort(unique(surveys$name))

  if (!is.na(survey_name))
    survey_names <- survey_names[survey_names == survey_name]

  ret <- list()

  for (name in survey_names) {

    name <- survey_name

    c_df <- surveys %>%
      filter(name == !!name, type == "C") %>%
      select(order, statement) %>%
      arrange(order)

    p_df <- surveys %>%
      filter(name == !!name, type == "P") %>%
      select(order, statement) %>%
      arrange(order)

    # get first row of data, all rows are the same for each survey
    scale_max <- surveys[surveys$name == name,]$scale_max[1]
    q_method <- surveys[surveys$name == name,]$q_method[1]

    ret[[length(ret)+1]] <- list(
      name = name,
      considerations = c_df,
      policies = p_df,
      scale_max = scale_max,
      q_method = q_method
    )

  }

  ret

}
