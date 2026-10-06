# --- Load libraries ---
#load_or_stop("tidyverse")


prep_loadings_data <- function(input_dt){
  message("Preparing the data...")
  dt <- input_dt |>
  # Remove rows with any missing data
    filter(dplyr::if_all(tidyselect::everything(), ~ !is.na(.))) |>
  # Rename the first column to variable  
    rename(variable = 1) |>
  # Rename the columns with loadings (lower case and remove the space)
    rename_with(~ str_remove_all(tolower(.), " "), starts_with("PC"))

  #Check all quantitative columns are actually numeric
  non_numeric_cols <- names(dt)[-1][!sapply(dt[-1], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    stop(
         "The following columns were expected to be numeric but are not: ",
         paste(non_numeric_cols, collapse = ", "),
         "\nCheck for stray text, units, or formatting issues in the CSV."
    )
  }

  message("Data ready for analysis!")
  return(dt)
}

