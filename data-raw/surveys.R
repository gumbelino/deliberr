library(readr)
library(readxl)
library(dplyr)

SURVEY_FILE <- "data-raw/surveys.xlsx"


# read the sheet names of the Excel file
survey_names <- excel_sheets(SURVEY_FILE)

# remove invalid and "template"
survey_names <- sort(survey_names[!grepl("^~", survey_names) &
                                    survey_names != "template"])

surveys <- list()

# Iterate over each sheet in the workbook
for (name in survey_names) {

  # Read the current sheet into a data frame
  df <- read_excel(SURVEY_FILE, sheet = name)

  # Check if required columns exist
  required_columns <- c("considerations", "policies", "scale_max", "q-method")
  missing_cols <- setdiff(required_columns, colnames(df))
  if (length(missing_cols) > 0) {
    cat(
      "Sheet",
      sn,
      "is missing the following columns:",
      paste(missing_cols, collapse = ", "),
      "\n\n"
    )
    next
  }

  # Calculate the number of non-NA rows in "considerations" column
  n_c <- sum(!is.na(df$considerations))

  # Calculate the number of non-NA rows in "policies" column
  n_p <- sum(!is.na(df$policies))

  # Extract integer values from "scale_max" column, assuming they are already integers
  scale_max <- as.integer(na.omit(df$scale_max))

  # Extract logical (boolean) values from "q-method" column
  q_method <- as.logical(na.omit(df$`q-method`))

  # get considerations
  c_df <- df %>%
    mutate(type = "C",
           order = considerations_order,
           statement = considerations) %>%
    select(type, order, statement) %>%
    filter(!is.na(statement))

  # get considerations
  p_df <- df %>%
    mutate(type = "P", order = policies_order, statement = policies) %>%
    select(type, order, statement) %>%
    filter(!is.na(statement))

  survey <- bind_rows(c_df, p_df)
  survey$name <- name
  survey$scale_max <- scale_max
  survey$q_method <- q_method

  surveys[[length(surveys)+1]] <- survey

}

surveys <- bind_rows(surveys)

usethis::use_data(surveys, overwrite = TRUE)


