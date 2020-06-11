#' The mEff function
#'
#' This function calculates the effective number of variables after taking into account correlation
#' @param variables dataframe containing the siginificant variables (dataCon)
#' @param key dataframe containing workName and group of every variable (key)
#' @param groupVars column within "key" containing groups of interest (e.g. "group2")
#' @param df dataframe (data)
#' @keywords effective number of variables
#' @examples mEff(key, outCon, "group2", dataCon)

mEff <- function(key, variables, groupVars, df) {

  groups <- key %>%  select(groupVars) %>% .[,1] %>% na.omit()  %>%  unique() %>% t

  outTot <- data.frame()

  for ( i in groups){
    varCor <- variables %>%
      mutate(term = gsub("0.*","0", term)) %>%
      merge(., key, by.x = "term", by.y = "workName") %>%
      rename(group3 = groupVars) %>%
      filter(group3 %in% i) %>%
      select(term) %>%
      unique() %>%
      t

    dfCor <- df %>%
      select(age0, sex0, varCor) %>%
      sapply(., as.numeric)

    tableCor <- partial.r(data = dfCor, c(3: (2 + length(varCor))), c(1:2))
    eigenValues <- eigen(tableCor)$values
    mEff <- 1 + (nrow(tableCor) - 1) * (1 - var(eigenValues)/nrow(tableCor)) %>%
      round(., 2)

    out <- cbind(i, mEff, ncol(varCor))
    outTot <- rbind(outTot, out)
  }

  names(outTot) <- c("group", "mEff", "mTot")
  outTot
}

