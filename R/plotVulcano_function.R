#' The plotVulcano function
#'
#' This function creates a Vulcano plot, displaying all estimates and p-values of the variables of interest
#' This function requires the individual variables to be classified into a grouping variable
#' @param dataframe dataframe with the p-values of all variables (outDig)
#' @param est variable which holds the estimates of the variables (estimate)
#' @param p variable which holds the p-value of the variables (pValue)
#' @param gr variable which holds info in which group the respective variable is
#' @keywords Vulcano
#' @examples plot2 <- plotVulcano(dfVulcano, "estimate", "pValue", "group2")


plotVulcano <- function(dataframe, est, p, gr) {
  ggplot(dataframe, aes_string(x=est, y = -log10(dataframe[,p]))) +
    geom_point(aes_string(color = gr), alpha = 0.60) +
    geom_vline(xintercept = 1, color = "grey40", linetype = "dashed") +
    guides(color=guide_legend(title = "Group")) +
    xlab("Hazard Ratio") +
    ylab("-Log10(p-value)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}
