plots_cox <- function(x){

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

    myform2 <- as.formula(paste("Surv(time, dmNewDig) ~ ", vars))
    cox <-  coxph(myform2, data = dataCon)$concordance[6]
    dfSum <- data.frame(i, cox)
    dfSumTot <- rbind(dfSumTot, dfSum)
  }

  sigVars %>%
    mutate(y = cumsum(total))

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
    select(term, total, n_of_vars, cox)

  dfSumTot <- data.frame()

  for (i in steps) {
    vars <- sigVars %>%
      filter(total >= i) %>%
      select(term) %>%
      t %>%
      paste(., collapse = " + ")

    myform2 <- as.formula(paste("Surv(time, dmNewDig) ~ ", vars))
    cox <-  summary(coxph(myform2, data = dataCon))$conf.int
    dfSum <- data.frame(i, cox)
    dfSumTot <- rbind(dfSumTot, dfSum)
  }

  dfSumTot <- dfSumTot %>%
    mutate(term = rownames(.)) %>%
    rename(total = i,
           hr = exp.coef.,
           ci_low = lower..95,
           ci_high = upper..95) %>%
    select(term, hr, ci_low, ci_high)

  work_2 <- dplyr::left_join(work_2, dfSumTot, by = c("term"))


  work_2 %>%
    separate(term, c("variable", "level"), sep = "0", remove = T, extra = "merge") %>%
    mutate(variable = paste(variable, "0", sep = "")) %>%
    merge(., key, by.x = "variable", by.y = "workName") %>%
    mutate(hr_ci = paste(round(hr,2), " (", round(ci_low, 2), "; ", round(ci_high, 2), ")", sep = "")) %>%
    select(outName, level, total, n_of_vars, cox, hr_ci) %>%
    arrange(desc(total), outName) %>%
    rename(`Number of times selected in model` = total, `Number of variables selected` = n_of_vars, `c-index` = cox, `HR (ci)` = hr_ci) %>%
    write.csv(., paste(name, "_included_cox.csv"), row.names = FALSE)


  work_2
}
