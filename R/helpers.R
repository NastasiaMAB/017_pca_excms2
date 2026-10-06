# Load libraries
#load_or_stop("tidyverse")


# --- Load libraries ---
load_or_stop <- function(pkg) {
  message("Looading pkg ", pkg, "...")
  ok <- tryCatch(
    {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      TRUE
    },
    error = function(e) {
      stop("Pkg ", pkg, " has not loaded because ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (ok) message("Pkg ", pkg, " succesfully loaded!")
  invisible(ok)
}


# --- Pick input file ---
file_select <- function(input_path = "data/data.csv"){
  message("Loading ", input_path, "...")
  dt <- read_delim(input_path, 
                   delim = ",",
                   na = c("", "NA", "na", "N/A", "n/a", "NaN"),
                   show_col_types = FALSE) |>
  # Remove column(s) with only NAs
  select(where(~ !all(is.na(.x)))) |>
  # Remove row(s) with only NAs 
  filter(!if_all(everything(), is.na))
  message("Data loaded!")
  return(dt)
}


# --- Define output paths ---
make_output_path <- function(output_path, filename, ext ) {
  output_path <- match.arg(output_path, c("f", "t", "si_f", "si_t"))
  ext <- match.arg(ext, c("p", "j", "c", "x"))

  output_path_map <- c(
                       f = "results/figures",
                       t = "results/tables",
                       si_f = "results/si/figures",
                       si_t = "results/si/tables"
  )

  ext_map <- c(
               p = ".pdf",
               j = ".jpg",
               c = ".csv",
               x = ".xlsx"
  )

  resolved_output_path <- output_path_map[[output_path]]
  resolved_ext <- ext_map[[ext]]

  dir.create(resolved_output_path, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(resolved_output_path, paste0(filename, resolved_ext))
  message("Creating output path: ", path)
  message("Output path created!")
  return(path)
}
