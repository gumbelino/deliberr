
roles <- read_csv("data-raw/roles.csv", show_col_types = FALSE)

usethis::use_data(roles, overwrite = TRUE)
