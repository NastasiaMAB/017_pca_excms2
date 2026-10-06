# --- Load libraries ---
# load_or_stop("ggplot2")
# load_or_stop("ggpubr")


make_scores_fig <- function(dt, pc_var){
  message("Creating the scores plot..")

  var1 <- names(dt)[1]
  var2 <- names(dt)[2]
  trt_col <- c("#628AF1", "#E75B5B", "#F9C14F", "#AFAFAF")
  soil_sym <- c(16, 17, 7, 8)

  p <- ggplot(dt, aes(x = pc1, y = pc2)) +
    geom_hline(yintercept = 0, colour = "gray65", linewidth = 0.1) +
    geom_vline(xintercept = 0, colour = "gray65", linewidth = 0.1) +
    geom_point(aes(colour = .data[[var2]], shape = .data[[var1]]), 
               size = 4, stroke = 1) +
    theme_classic() +
    labs(x = pc_var[1], 
         y = pc_var[2]) +
    scale_shape_manual(values = soil_sym) +
    scale_color_manual(values = trt_col) +
    scale_fill_manual(values = trt_col) +
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.line = element_line(linewidth = 0.2),
          legend.title = element_blank(),
          legend.text = ggtext::element_markdown(size = 14),
          legend.position = "right")
  print(p)

  return(p)
}

