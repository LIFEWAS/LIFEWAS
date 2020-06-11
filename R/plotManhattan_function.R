#' The plotManhattan function
#'
#' This function creates a Manhattan plot, displaying all p-values of the variables of interest
#' This function requires the individual variables to be classified into a grouping variable
#' @param dataframe dataframe with the p-values of all variables (outDig)
#' @param gr variable which holds info in which group the respective variable is
#' @param significant variable which holds info in how many pathways a variable was significant (one, two, none; models variable)
#' @param p variable which holds the p-value of the variables (pValue)
#' @keywords Manhattan
#' @examples plot1 <- plotManhattan(outDig, "group", "models", "pValue"); plot1; plot1 + scale_y_continuous(expand = c(0,0), limits = c(0, 50)); ggsave("manhattan.png")
#' plotManhattan()


plotManhattan <- function(dataframe, gr, significant, p){

  jitter <- position_jitter(width = 0.1, height = 0.2)

  ggplot(dataframe, aes_string(x=gr, y=-log10(dataframe[,p]))) +
    geom_point(aes_string(color = significant), alpha = 0.75, size = 1.25, position = jitter) +
    scale_color_manual(values = c(two = "darkgreen", one =  "orange", none = "red")) +
    labs(x=NULL, y= expression(-log [10] * "(" * italic(p) * "-value)")) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 60, size = 8, vjust = 0.5))
}
