#' The sharing legend function
#'

grid_arrange_shared_legend <- function(..., nrow = length(list(...)), ncol = 1, position =  c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)

  g <- ggplotGrob(plots[[3]] +
                    theme(legend.position = position))$grobs

  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)

  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))

  gl <- c(gl, ncol = ncol, nrow =  nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend, ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  invisible(combined)
}
