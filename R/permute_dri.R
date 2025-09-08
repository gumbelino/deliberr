
#' Tests whether the links between considerations and policy preferences are
#' consistent or due to chance.
#'
#' @param data raw dri survey dataframe
#' @param iterations number permutations to generate. Default = 10000
#' @param verbose boolean flag to print time of permutation. Default = FALSE
#' @param summary whether to return the raw data or summary of test results
#'
#' @import dplyr
#'
#' @returns dataframe with permutation test results, raw or summarized
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
#' permute_dri(data, iterations = 100)
#'
permute_dri <- function(data, iterations = 10000, verbose = FALSE, summary = TRUE) {

  res <- list()

  # GET OBSERVED DRI
  ic <- get_dri_ic(data)
  obs_dri <- get_dri(ic)

  raw <- tibble(
    dri = obs_dri,
    source = "observed",
  )

  # RUN PERMUTATION
  # get preferences
  pref_cols <- grep("^P\\d", names(data), value = TRUE)
  shuffled_data <- data
  dri_shuffle <- list()

  time_start <- Sys.time()

  # permutation loop
  for (i in 1:iterations) {

    # shuffle preferences
    shuffled_data[pref_cols] <-
      shuffled_data[sample(1:nrow(shuffled_data)), pref_cols]

    # get DRI
    ic <- get_dri_ic(shuffled_data)
    dri <- get_dri(ic)

    dri_shuffle[[length(dri_shuffle) + 1]] <- tibble(
      dri,
      source = "permutation",
      iteration = i
    )

  }

  time_end <- Sys.time()

  elapsed_time <- format(difftime(time_end, time_start, units = "auto"), digits = 3)

  if (verbose) cat(iterations, "permutations completed in", elapsed_time, "\n")

  # merge permutation data
  dri_shuffle <- bind_rows(dri_shuffle)

  raw <- bind_rows(raw, dri_shuffle)

  # calculate p
  p <- nrow(dri_shuffle %>% filter(dri >= obs_dri)) / nrow(dri_shuffle)

  #
  res <- tibble(
    n = nrow(data),
    obs_dri = obs_dri,
    n_perm = nrow(dri_shuffle),
    mean_perm_dri = mean(dri_shuffle$dri, na.rm = TRUE),
    p = p,
  )

  if (summary) return(res) else return (raw)

}
