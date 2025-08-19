#' get DRI IC
#'
#' @param data A dataframe
#'
#' @returns A dataframe
#' @export
#' @import dplyr
#' @importFrom stats cor
#' @examples
#' get_dri_ic(data)
get_dri_ic <- function(data) {

  # create separate dataframes for C and P columns
  df_c <- data %>% select(pnum, matches("^C\\d+$") & where(~!all(is.na(.))))
  df_p <- data %>% select(pnum, matches("^P\\d+$") & where(~!all(is.na(.))))

  # transpose the data and make pnum the row names for correlation calculation
  df_c_t <- df_c %>% column_to_rownames("pnum") %>% t()
  df_p_t <- df_p %>% column_to_rownames("pnum") %>% t()

  # calculate correlation matrices and reshapes them to a long format
  corr_c <- cor(df_c_t, method = "spearman")
  corr_p <- cor(df_p_t, method = "spearman")

  # convert matrices to dataframes and add a column for pnum
  df_corr_c <- as.data.frame(corr_c) %>%
    mutate(pnum1 = as.numeric(rownames(.)))

  df_corr_p <- as.data.frame(corr_p) %>%
    mutate(pnum1 = as.numeric(rownames(.)))

  # reshape from wide to long format
  long_corr_c <- df_corr_c %>%
    pivot_longer(
      cols = -pnum1,
      names_to = "pnum2",
      values_to = "ccor",
      names_transform = as.numeric
    )

  long_corr_p <- df_corr_p %>%
    pivot_longer(
      cols = -pnum1,
      names_to = "pnum2",
      values_to = "pcor",
      names_transform = as.numeric
    )

  # join the two dataframes, filter for unique pairs, and create the final output
  final_df <- long_corr_c %>%
    inner_join(long_corr_p, join_by(pnum1, pnum2)) %>%
    filter(pnum1 < pnum2) %>% # filter out duplicate pairs and self-correlations
    mutate(pnums = paste0(pnum1, "-", pnum2)) %>%
    select(pnums, pnum1, pnum2, ccor, pcor)

  return(final_df)

}
