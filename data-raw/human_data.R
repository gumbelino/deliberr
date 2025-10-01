library(readr)

human_data <- read_csv("data-raw/human_data.csv", show_col_types = FALSE)

usethis::use_data(human_data, overwrite = TRUE)
