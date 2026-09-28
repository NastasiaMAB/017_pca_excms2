# --- Load libraries ---
load_or_stop <- function(pkg) {
  message("Looading pkg ", pkg, "...")
  ok <- tryCatch(
    {
      suppressPackageStartupMessages(library(pkg, character.only = T))
      TRUE
    },
    error = function(e) {
      stop("Pkg ", pkg, " has not loaded because ",
           conditionMessage(e), call. = F)
    }
  )
  if (ok) message("Pkg ", pkg, " succesfully loaded!")
  invisible(ok)
}


# --- Pick input file ---
file_select <- function(input_path = "data/data.csv"){
  dt <- read_delim(input_path, 
                   delim = ",",
                   na = c("", "NA", "na", "N/A", "n/a", "NaN"),
                   show_col_types = F) |>
  # Remove column(s) with only NAs
  select(where(~ !all(is.na(.x)))) |>
  # Remove row(s) with only NAs 
  filter(!if_all(everything(), is.na))
  message("Data loaded!")
  return(dt)
}


# --- Define output paths ---
make_output_path <- function(output_path, filename, ext){
  dir.create(output_path, showWarnings = F, recursive = T)
  path <- file.path(output_path, paste0(filename, ext))
  message("Output path created!")
  return(path)
}

