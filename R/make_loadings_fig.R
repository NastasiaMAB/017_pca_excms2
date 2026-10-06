# --- Load libraries ---
#load_or_stop("ggplot2")
#load_or_stop("ggrepel")
#load_or_stop("ggpubr")


make_loadings_fig <- function(dt, pc_var){
  message("Preparing the loadings plot...")
  p1 <- ggplot(dt, aes(x = 0, y = 0)) + 
    geom_segment(aes(xend = pc1, yend = pc2),
                 arrow = arrow(length = unit(0.1, "cm")), 
                 linewidth = 0.4, colour = "grey40") +
    geom_text_repel(data = dt, aes(x = pc1, y = pc2, label = variable),
                    min.segment.length = 0,
                    segment.color = "blue",
                    segment.size = 0.3,
                    size = 4) +
                    # For a version that shows all the variable 
                    # in geom_text_repel() 
                    # set max.overlaps = 20, force_pull = 0 and force = 5
    coord_equal() +
    coord_cartesian(clip = "off") +
    labs(x = pc_var[1], y = pc_var[2]) +
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
    labs(x = pc_var[1], y = pc_var[2]) +
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

