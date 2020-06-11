#' The correlationPlot function
#'
#' This function creates a correlation plot of the variables of interest, using partial Pearson correlations and adjusting for age and sex
#' The variables in the plot are ordered using a hierargical clustering method.
#' This function outputs a correlation plot and a correlation table. Both are automatically saved in the workspace.
#' @param variables dataframe containing the siginificant variables
#' @param df dataframe (data)
#' @param key dataframe containing
#' @param nColors number of colors per direction (4 per direction equals 8 colors in total)
#' @keywords correlation matrix, correlation plot, hierargical clustering
#' @examples plot3 <-  correlating(outCon, dataOut, key, 4)

correlating <- function(variables, df, key, nColors){
  varCor <- variables %>%
    select(term) %>%
    mutate(term = gsub("0.*","0", term)) %>%
    unique() %>%
    merge(., key, by.x  = "term", by.y = "workName", all.x = T) %>%
    select(outName, color) %>%
    t

  dfCor <- df %>%
    select(age0, sex0, varCor[1,]) %>%
    sapply(., as.numeric)

  tableCor <- partial.r(data = dfCor, c(3:(length(varCor)/2+2)), c(1:2))

  heatmapColors <- function(numColors = nColors){
    c1 <- rainbow(numColors, v=seq(0.5, 1, length=numColors), s=seq(1, 0.2, length = numColors), start = 4/6, end = 4.0001/6);
    c2 <- rainbow(numColors, v=seq(0.5, 1, length=numColors), s=seq(1, 0.2, length = numColors), start = 1/6, end = 1.0001/6);
    c3 <- c(c1, rev(c2));
    return(c3)
  }

  png("cor_plot.png", width = 2000, height = 2000)
  heatmap.2(tableCor,
            trace = "none",
            margins = c(37,37),
            col = heatmapColors(nColors),
            colRow = varCor[2,],
            colCol = varCor[2,],
            srtCol = 70,
            srtRow = 10,
            cexRow = 2.5,
            cexCol = 2.5,
            dendogram = "both",
            Rowv = TRUE,
            Colv = TRUE,
            key.title = "",
            key.xlab = "",
            key.ylab = "",
            keysize = 1,
            key.par = list(cex = 2.2),
            symbreaks = T,
            symkey = T,
            density.info = "density",
            densadj = 2.5)
  graphics.off()

  out <- corr.p(tableCor, n = (96534 - 2))
  write.csv(tableCor, "cor_table.csv")
  
  tableCor
}
