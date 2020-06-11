#' The impact_log function for continuous traits
#'

impact_log <- function(x, y){

  name <- gsub("_.*", "", deparse(substitute(x)))

  sigVars <- x %>%
    mutate(term = gsub("0.*", "0", term)) %>%
    select(term) %>%
    t

  sigVars_one <-  paste(sigVars, collapse = " + ")
  myform <-  as.formula(paste("dmNewDig ~", sigVars_one))
  fullC <- concordance(glm(as.formula(paste("dmNewDig ~", sigVars_one)) , family = "binomial", data  = dataCon))$concordance

  dfSumTot <- data.frame()

  for (i in sigVars) {
    myform2 <- update(myform, as.formula(paste('~ . - ',  i)))
    log <-  concordance(glm(myform2, family = "binomial", data = dataCon))$concordance
    dfSum <- data.frame(i, log)
    dfSumTot <- rbind(dfSumTot, dfSum)
  }

  impact <- dfSumTot %>%
    mutate(diffCindex = abs((log - fullC) / fullC)*100) %>%
    rename(term = i, cIndex = log) %>%
    arrange(desc(diffCindex)) %>%
    unique %>%
    merge(., key, by.x = "term", by.y = "workName")

  impactOut <- impact %>%
    select(outName, cIndex, diffCindex) %>%
    arrange(desc(diffCindex))
  rownames(impactOut) <- c()
  names(impactOut) <- c("Excluded variable", "New C-index", "Absolute difference (perc)")
  write.csv(impactOut, paste(name, "_impact.csv"), row.names = FALSE)

  impact %>%
    mutate(model = y)
}
