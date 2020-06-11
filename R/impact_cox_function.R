#' The impact_cox function for continuous traits
#'

impact_cox <- function(x, y){

  name <- gsub("_.*", "", deparse(substitute(x)))

  sigVars <- x %>%
    mutate(term = gsub("0.*", "0", term)) %>%
    select(term) %>%
    t

  sigVars_one <-  paste(sigVars, collapse = " + ")
  myform <-  as.formula(paste("Surv(time, dmNewDig) ~", sigVars_one))
  fullC <- coxph(as.formula(paste("Surv(time, dmNewDig) ~", sigVars_one)) , data  = dataCon)$concordance[6]

  dfSumTot <- data.frame()

  for (i in sigVars) {
    myform2 <- update(myform, as.formula(paste('~ . - ',  i)))
    cox <-  coxph(myform2, data = dataCon)$concordance[6]
    dfSum <- data.frame(i, cox)
    dfSumTot <- rbind(dfSumTot, dfSum)
  }

  impact <- dfSumTot %>%
    mutate(diffCindex = abs((cox - fullC) / fullC)*100) %>%
    rename(term = i, cIndex = cox) %>%
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
