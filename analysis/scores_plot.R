# Source helper functions (contains the load_or_stop function)
source("R/helpers.R", echo = FALSE)
# Source libraries
invisible(lapply(
                 c(
                   "tidyverse",
                   "glue",
                   "ggpubr"
                   ),
                 load_or_stop
                 ))
# Source analysis functions
invisible(lapply(
                 c(
                   "R/make_axis_titles.R",
                   "R/prep_scores_dt.R",
                   "R/make_scores_fig.R"
                   ),
                 source,
                 echo = FALSE))
# Select file
score <- file_select("data/score.csv")
message("Preview of scores data:")
print(head(score, n = 10L))
variance <- file_select("data/variance.csv")
message("Preview of pc variance:")
print(head(variance, n = 5L))
# Create output_path for the figure
score_fig_path <- make_output_path("f", "scores", "j")
# Prep data for the figure
score_prep_dt <- prep_score_data(score)
message("Preview of prep scores data:")
print(head(score_prep_dt, n = 10L))
score_axis_titles <- make_axis_title(variance)
message("Preview of pc variance/figure axis titles:")
print(score_axis_titles)
# Make the figure
score_fig <- make_scores_fig(score_prep_dt, score_axis_titles) 
# Save the figure
message("Saving the figure...")
ggsave(score_fig_path, score_fig, 
       width = 7, height = 7, 
       units = "in", dpi = 300)
message("Figure saved!")
