plots_log <- function(x){

  name <- deparse(substitute(x))

  sigVars <- x %>%
    mutate(term = gsub("0.*", "0", term))

  steps <- sigVars %>%
    select(total) %>%
    unique %>%
    t

  dfSumTot <- data.frame()

  for (i in steps) {
    vars <- sigVars %>%
      filter(total >= i) %>%
      select(term) %>%
      t %>%
      paste(., collapse = " + ")

    myform2 <- as.formula(paste("dmNewDig ~", vars))
    log <-  concordance(glm(myform2, family = "binomial", data = dataCon))$concordance
    dfSum <- data.frame(i, log)
    dfSumTot <- rbind(dfSumTot, dfSum)
  }


  sigVars %>%
    mutate(y = cumsum(total))

  class(sigVars$total)

  work <- steps %>%
    t %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(i = total) %>%
    mutate(n_of_vars = as.numeric(rowname)) %>%
    merge(dfSumTot) %>%
    arrange(n_of_vars)

  #assuming the second step only contains one variable
  work$n_of_vars[work$n_of_vars == 1]  <- (work$n_of_vars[2] - 1)

  work_2 <- dplyr::full_join(x, work, by = c("total"= "i")) %>%
    select(term, total, n_of_vars, log)

  work_2 %>%
    separate(term, c("variable", "level"), sep = "0", remove = T, extra = "merge") %>%
    mutate(variable = paste(variable, "0", sep = "")) %>%
    merge(., key, by.x = "variable", by.y = "workName") %>%
    select(outName, level, total, n_of_vars, log) %>%
    arrange(desc(total), outName) %>%
    rename(`Number of times selected in model` = total, `Number of variables selected` = n_of_vars, `c-index` = log) %>%
    write.csv(., paste(name, "_included_cox.csv"), row.names = FALSE)


  work_2
}
