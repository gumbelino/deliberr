#' Summarize the results of a permutation test
#'
#' @param perms results of the permutation test
#' @param type which type of statistics to summarize (e.g., common, robust, mean)
#'
#' @importFrom rstatix get_summary_stats
#' @import dplyr
#' @importFrom rlang .data
#'
#' @returns summary of permutation test
#' @seealso [rstatix::get_summary_stats()] for values of "type".
#' @export
#'
#' @examples
#'
#' library(tibble)
#' data <- tibble(
#'   pnum = c(1,2,3,4),
#'   C1 = c(1,2,3,4),
#'   C2 = c(2,3,4,3),
#'   C3 = c(3,2,4,2),
#'   C4 = c(4,3,3,1),
#'   P1 = c(1,2,3,3),
#'   P2 = c(2,3,1,2),
#'   P3 = c(3,1,2,1),
#' )
#'
#' perms <- permute_dri(data, iterations = 100, summary = FALSE)
#' summarize_perm_dri(perms)
#'
summarize_perm_dri <- function(perms, type = "common") {

  # get observed dri
  obs_dri <- perms[perms$source == "observed",]$dri

  # get all permutations
  perm_dri <- perms[perms$source == "permutation",]

  # get permutation summary
  perm_summ <- perm_dri %>%
    mutate(perm_dri = .data$dri) %>%
    select(perm_dri) %>%
    get_summary_stats(type = type) %>%
    select(-.data$variable)

  # calculate p
  p <- nrow(perm_dri %>% filter(.data$dri >= obs_dri)) / nrow(perm_dri)

  # compile summary
  summ <- tibble(
    obs_dri,
    p,
    perm_summ
  )

  return(summ)

}
