
prompts <- read_csv("data-raw/prompts.csv", show_col_types = FALSE)

usethis::use_data(prompts, overwrite = TRUE)
