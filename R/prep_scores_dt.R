# --- Load libraries ---
#load_or_stop("tidyverse")
#load_or_stop("ggpubr")
#load_or_stop("glue")


prep_score_data <- function(score){
  message("Preparing the data...")
  #Check all quantitative columns are actually numeric
  non_numeric_cols <- names(score)[!c(1, 2)][!sapply(score[-c(1, 2)], 
                                                     is.numeric)]
  if (length(non_numeric_cols) > 0) {
    stop(
         "The following columns were expected to be numeric but are not: ",
         paste(non_numeric_cols, collapse = ", "),
         "\nCheck for stray text, units, or formatting issues in the CSV."
    )
  }

  dt <- score |>
  # Rename the columns with PC to lower case and remove the space
    rename_with(~ str_remove_all(tolower(.), " "), starts_with("PC")) |>
    # Make values in column 1 and column 2 as factor
    mutate(across(1:2, as.factor),
           # Rename factor levels of trt
           trt_order = if_else(trt == "bs", 2,
                         if_else(trt == "cr", 1, 
                                 if_else(trt == "pp", 3,
                                         4))),
           trt = fct_recode(trt, 
                            "B. subtilis" = "bs",
                            "C. rosea" = "cr", 
                            "P. polymyxa" = "pp",
                            "Control" = "cont"),
    trt_new_label_rule = trt == "Control",
    trt_new_label = if_else(trt_new_label_rule, glue("{trt}"),
                        glue("<i>{trt}</i>")),
    trt_new_label = fct_reorder(trt_new_label, trt_order),
           soil = fct_recode(soil, 
                            "AT" = "Austria",
                            "DE" = "Germany", 
                            "IT" = "Italy",
                            "PL" = "Poland"),
           soil_order = if_else(soil == "AT", 1,
                                if_else(soil == "DE", 2, 
                                        if_else(soil == "IT", 3, 4))),
           soil = fct_reorder(soil, soil_order)) |>
    relocate(trt_new_label, .after = soil)

  message("Data ready for analysis!")
  return(dt)
}
