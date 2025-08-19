#' Plot DRI IC
#'
#' @param data IC
#' @param x name of x variable
#' @param y name of y variable
#' @param title title of the plot (default = NA)
#' @param suffix string pre, post (default = NA)
#' @param DRI numeric value (default = NA)
#' @import ggplot2 grid
#'
#' @returns plot
#' @export
#'
#' @examples
#' dri_plot(data, x, y)
dri_plot <- function(data,
                     x,
                     y,
                     title = NA,
                     suffix = NA,
                     DRI = NA) {

  # create a grid
  grob <- grobTree(textGrob(
    paste0("DRI = ", DRI),
    x = 0.1,
    y = 0.9,
    hjust = 0,
    gp = gpar(
      col = "red",
      fontsize = 13,
      fontface = "italic"
    )
  ))

  plot <-
    ggplot(data, aes(x = get(x), y = get(y))) +
    geom_jitter(
      width = 0.02,
      height = 0.02,
      show.legend = TRUE,
      col = rgb(0.1, 0, 1, 0.6),
      lwd = 3
    ) +
    xlim(-1.1, 1.1) + ylim(-1.1, 1.1) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "white",
      lwd = 1
    ) +
    geom_hline(yintercept = 0,
               color = "white",
               lwd = 1) +
    geom_vline(xintercept = 0,
               color = "white",
               lwd = 1) +
    labs(x = "Intersubjective Agreement - Considerations", y = "Intersubjective Agreement - Preferences") +
    ggtitle(paste0(title, ": ", suffix)) +
    annotation_custom(grob) +
    geom_density_2d_filled(inherit.aes = T, alpha = 0.3) +
    geom_density_2d(linewidth = 0.25, colour = "black") +
    theme(legend.position = "none")

  return(plot)
}
