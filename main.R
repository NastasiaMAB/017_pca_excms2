# source("R/set_up_pj_lib.R", echo= FALSE)
# resolve graphic device for both source() and Rscript
if (interactive()) {
  dev.new()
  plot.new()  # forces device to fully initialize
} else {
  pdf(NULL)
}
source("analysis/loadings_plot.R", echo = FALSE)
source("analysis/scores_plot.R", echo = FALSE)
