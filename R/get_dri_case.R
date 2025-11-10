#' Get DRI Case
#'
#' Get the DRI of the specified case in human data
#'
#' @param case a character string specifying the name of the case in human_data
#' @param adjusted a logical indicating whether you want the original or adjusted
#' DRI formula
#' @param method a character string specifying the method for statistical testing,
#' must be one of "wilcox" (default) or "t.test"
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "greater" (default), "two.sided" or "less".
#' You can specify just the initial letter.
#' @param data the dataset to extract data from
#'
#' @returns a tibble with with the following components: \code{case}, \code{pre},
#' \code{post}, \code{delta}, \code{p_value}, and \code{significance}.
#'
#' @export
#'
#'
#' @importFrom stats wilcox.test t.test
#' @import tibble dplyr
#' @importFrom rlang .data
#'
#' @examples
#'
#' get_dri_case("Activate")
#'
#'
get_dri_case <- function(case, adjusted = TRUE, method = "wilcox",
                         alternative = "greater", data = NULL) {

  if (is.null(data)) data <- deliberr::human_data

  # get pre/post data
  data_pre <- data %>% filter(case == !!case, .data$stage_id == 1)
  data_post <- data %>% filter(case == !!case, .data$stage_id == 2)

  # get ic
  ic_pre <- get_dri_ic(data_pre)
  ic_post <- get_dri_ic(data_post)

  # calculate individual dri
  dri_ind_pre <- get_dri_ind(ic_pre, adjusted)
  dri_ind_post <- get_dri_ind(ic_post, adjusted)

  # calculate dri
  dri_pre <- get_dri(ic_pre, adjusted)
  dri_post <- get_dri(ic_post, adjusted)

  # calculate delta
  dri_delta <- dri_post - dri_pre

  # test pre/post difference in means
  if (method == "wilcox") {
    res <- wilcox.test(dri_ind_post$dri,
                       dri_ind_pre$dri,
                       paired = TRUE,
                       alternative = alternative)
    p_value <- res$p.value
  } else if (method == "t.test") {
    res <- t.test(dri_ind_post$dri,
                  dri_ind_pre$dri,
                  paired = TRUE,
                  alternative = alternative)
    p_value <- res$p.value
  } else {
    stop("Method", method, "not supported.")
  }

  # compile result
  dri_case <- tibble(case,
                     pre = dri_pre,
                     post = dri_post,
                     delta = dri_delta,
                     p_value) %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE            ~ "n.s."
      )
    )

  return(dri_case)

}
