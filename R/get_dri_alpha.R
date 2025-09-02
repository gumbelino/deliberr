#' Get DRI Cronbach's Alpha
#'
#' @param data the raw survey data
#'
#' @returns a dataframe with alpha_c, alpha_p, and alpha_all with values of
#' Cronbach's alpha for considerations, policy preferences, and both, respectively.
#' @export
#' @import dplyr
#' @import tibble
#' @importFrom psych alpha
#' @examples
#' get_dri_alpha(data)
get_dri_alpha <- function(data) {

  # Calculate Cronbach's Alpha for considerations
  considerations_data <- data %>%
    select(matches("^C\\d+$") & where(~!all(is.na(.))))

  if (nrow(considerations_data) > 1) {

    # Check if policies are all equal (no variance)
    # this can happen when there are few iterations
    c_all_equal <- all(apply(considerations_data, 1, function(row)
      all(row == considerations_data[1, ], na.rm = TRUE)), na.rm = TRUE)

    # NOTE: assign alpha = 1, which should NOT exist!
    if (c_all_equal) {
      alpha_considerations <- 1
    } else {
      alpha_considerations <- alpha(
        considerations_data,
        check.keys = TRUE,
        warnings = FALSE,
      )$total$raw_alpha
    }
  } else {
    alpha_considerations <- NA
  }

  # Calculate Cronbach's Alpha for policies
  policies_data <- data %>%
    select(matches("^P\\d+$") & where(~!all(is.na(.))))

  if (nrow(policies_data) > 1) {

    # Check if policies are all equal (no variance)
    # this can happen when there are few iterations
    p_all_equal <- all(apply(policies_data, 1, function(row)
      all(row == policies_data[1, ], na.rm = TRUE)), na.rm = TRUE)

    # NOTE: assign alpha = 1, which should NOT exist!
    if (p_all_equal) {
      alpha_policies <- 1
    }

    # normal case, calculate alpha
    else {
      alpha_policies <- alpha(
        policies_data,
        check.keys = TRUE,
        warnings = FALSE,
      )$total$raw_alpha
    }
  } else {
    alpha_policies <- NA
  }

  if (nrow(policies_data) > 1 && nrow(considerations_data) > 1) {
    all_data <- cbind(considerations_data, policies_data)
    alpha_all <- alpha(
      all_data,
      check.keys = TRUE,
      warnings = FALSE,
    )$total$raw_alpha
  } else {
    alpha_all <- NA
  }

  # Store the results in the list
  result <- tibble(
    alpha_c = alpha_considerations,
    alpha_p = alpha_policies,
    alpha_all = alpha_all,
  )

  return(result)

}
