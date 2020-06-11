#' The dummy function
#'
#' This document describes to functions. The first function, to_con divides the variables of interest in factors and continuous variables.
#' All variables with less than 20 unique values are categorized as factor. Numerical variables are recentered and scaled using standard deviation.
#' The second function, to_dig succcessively creates dummy variables of all factors.
#' @param exclude Variables which do not need to be converted.
#' @param dataframe Dataframe which is to be used as input.
#' @keywords dummy, dummie
#' @examples exeptions <- c("PSEUDOIDEXT", "idFU", "group", "dmNew", "dmNewDig", "time"); dataCon <- to_con(dataGrouped, exeptions); dataDig <- to_dig(dataGrouped, exeptions)
#' to_con(), to_dig()


#does not create dummy variables of multilevel factors
to_con <- function(dataframe, exclude){
  t1 <- select(dataframe, -exclude) %>%
    lapply(function(x) length(unique(x)))

  numeric <- names(t1[which(t1 >= 20)])
  factors <- names(t1[which(t1 <= 3)])
  factorsMultiLevel <- names(t1[which(t1 > 3 & t1 < 20)])

  dataframe <- dataframe %>%
    mutate_at(numeric, funs(c(scale(.))))

  dataframe[factors] <- lapply(dataframe[factors], factor)
  dataframe[factorsMultiLevel] <- lapply(dataframe[factorsMultiLevel], factor)
  dataframe
}

#creates dummy variables of multilevel factors
to_dig <- function(dataframe, exclude){
  t1 <- select(dataframe, -exclude) %>%
    lapply(function(x) length(unique(x)))

  numeric <- names(t1[which(t1 >= 20)])
  factors <- names(t1[which(t1 <= 3)])
  factorsMultiLevel <- names(t1[which(t1 > 3 & t1 < 20)])

  dataframe <- dataframe %>%
    mutate_at(numeric, funs(c(scale(.))))

  dataframe[factors] <- lapply(dataframe[factors], factor)
  dataframe[factorsMultiLevel] <- lapply(dataframe[factorsMultiLevel], factor)
  dataframe <- createDummyFeatures(dataframe, cols = factorsMultiLevel)
}
