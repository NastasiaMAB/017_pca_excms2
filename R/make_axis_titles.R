# Load libraries
#load_or_stop("tidyverse")
#load_or_stop("glue")


make_axis_title <- function(variance){
  pc_var <- variance |>
    # Remove column with eigenvalues
    dplyr::select(!eigenvalue) |>
    # Columns become rows
    pivot_wider(names_from = pc, values_from = variance)

  # Set new names for the dataframe with the pc variance
  new_names <- c(
                 glue("PC{names(pc_var)} ({round(unlist(pc_var[1, ]))}%)")
  )

   axis_title <- pc_var |>
    # set new names to the dataframe with pca variance
    set_names(new_names) |>
    # Keep only the row names with the pc variance
    names()
    
  message("Axis titles created!")
  return(axis_title)
}

