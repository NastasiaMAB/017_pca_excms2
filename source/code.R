# --- Load libraries ---
suppressPackageStartupMessages({
  library(tidyverse) 
  library(ggrepel)
  library(ggpubr)
})
message("Tidyverse, ggrepel and ggpubr packages loaded!")


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


# --- Make the figure ---
# 1. Prep the data
prep_data <- function(input_dt){
  # Remove rows with any missing data
  dt <- input_dt |>
    filter(dplyr::if_all(tidyselect::everything(), ~ !is.na(.))) |>
  # Rename the first column to variable  
    rename(variable = 1) |>
  # Rename the columns ith loadings (lower case and remoe the space)
    rename_with(~ str_remove_all(tolower(.), " "), starts_with("PC")) |>
  # Select the columns with PC1 and PC2 loadings
    dplyr::select(1:3)
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

# 2. Make the figure 
make_fig <- function(dt){
  p1 <- ggplot(dt, aes(x = 0, y = 0)) + 
    geom_segment(aes(xend = pc1, yend = pc2),
                 arrow = arrow(length = unit(0.1, "cm")), 
                 linewidth = 0.4, colour = "grey40") +
    geom_text_repel(data = dt, aes(x = pc1, y = pc2, label = variable),
                    min.segment.length = 0,
                    segment.color = "blue",
                    segment.size = 0.3,
                    size = 4) +
                    # Remove the three lines below for the version with not
                    # all variable
                    # max.overlaps = 20,
                    # force_pull = 0,
                    # force = 5) +
    coord_equal() +
    coord_cartesian(clip = "off") +
    labs(x = "PC1 (70%)", y = "PC2 (16%)") +
    theme(
          axis.line = element_line(color = "black"),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 16)
    ) +
    ggtitle("a")

  p2 <- ggplot(dt, aes(x = 0, y = 0)) + 
    geom_segment(aes(xend = pc1, yend = pc2),
                 arrow = arrow(length = unit(0.1, "cm")), 
                 linewidth = 0.4, colour = "grey40") +
    geom_text_repel(data = dt, aes(x = pc1, y = pc2, label = variable),
                    min.segment.length = 0,
                    segment.color = "blue",
                    segment.size = 0.3,
                    size = 4) +
    coord_equal() +
    coord_cartesian(xlim = c(-0.07, 0.06), 
                    ylim = c(-0.1, 0.03)) +
    labs(x = "PC1 (70%)", y = "PC2 (16%)") +
    theme(
          axis.line = element_line(color = "black"),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 16)
    ) +
    ggtitle("b")

  p <- ggarrange(p1, p2, ncol=1, nrow = 2)
  print(p)
  return(p)
}

# Select file
data <- file_select()
message("Preview of data")
print(head(data, n = 10L))
# Create output_path for the figure
fig_path <- make_output_path("figures", "loadings_noall", ".jpeg")
# Prep data for the figure (aka select PC1 and PC2)
fig_dt <- prep_data(data)
message("Preview of prep data")
print(head(fig_dt, n = 10L))
# Make the figure
fig <- make_fig(fig_dt) 
# Save the figure
ggsave(fig_path, fig, width = 6.39, height = 8.71, units = "in", dpi = 300)
