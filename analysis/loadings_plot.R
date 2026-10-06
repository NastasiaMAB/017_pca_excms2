# Source helper functions (contains the stop_and_load function)
source("R/helpers.R", echo = FALSE)
# Source libraries
invisible(lapply(
                 c(
                   "tidyverse",
                   "glue",
                   "ggrepel",
                   "ggpubr"
                   ),
                 load_or_stop
                 ))
# Source analysis functions
invisible(lapply(
                 c(
                   "R/make_axis_titles.R",
                   "R/prep_loadings_dt.R",
                   "R/make_loadings_fig.R"
                   ),
                 source,
                 echo = FALSE))
# Select file
loadings <- file_select("data/loadings.csv")
message("Preview of loadings data")
print(head(loadings, n = 10L))
variance <- file_select("data/variance.csv")
message("Preview of variance data")
print(head(variance, n = 5L))
# Create output_path for the figure
loadings_fig_path <- make_output_path("f", "loadings", "j")
# Prep data for the figure
loadings_fig_dt <- prep_loadings_data(loadings)
message("Preview of loadings prep data")
print(head(loadings_fig_dt, n = 10L))
loadings_axis_titles <- make_axis_title(variance)
message("Preview of pc variance/figure axis title")
print(loadings_axis_titles)
# Make the figure
loadings_fig <- make_loadings_fig(loadings_fig_dt, loadings_axis_titles) 
# Save the figure
message("Saving the figure")
ggsave(loadings_fig_path, loadings_fig, 
       width = 6.39, height = 8.71, 
       units = "in", dpi = 300)
