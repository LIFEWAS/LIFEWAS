#' The Characteristics function
#'
#' This function creates characteristics (mean(sd) or %) using the createtableone function, displaying all summary statistics of the variables of interest
#' Automatically saves the output as a csv file.
#' @param dataframe dataframe with the p-values of all variables (outDig)
#' @param key dataframe with two columns; 1: list of variable names with (current) working name. 2: list of variable names with (desired) output name.
#' @param exclude variables which to exclude from the summary statistics
#' @keywords Characteristics
#' @examples characteristics <- char(dataGrouped, "key", "exclude"); plot1; plot1 + scale_y_continuous(expand = c(0,0), limits = c(0, 50)); ggsave("manhattan.png")
#' plotManhattan()

char <- function(dataframe, key, exclude){

old <- key[1] %>% t
new <- key[2] %>% t

dfTable <- copy(dataframe)
dfTable <- dfTable %>% setnames(old, new, skip_absent = T)

t1 <- select(dfTable, -exclude) %>%
  lapply(function(x) length(unique(x)))

numeric <- names(t1[which(t1 >= 20)])
factors <- names(t1[which(t1 <= 3)])
factorsMultiLevel <- names(t1[which(t1 > 3 & t1 < 20)])
dfTable[factorsMultiLevel] <- lapply(dfTable[factorsMultiLevel], factor)

table1 <- print(CreateTableOne(vars = c(numeric, factors, factorsMultiLevel), data = dfTable, factorVars = c(factors, factorsMultiLevel)), quote = FALSE, noSpaces = TRUE, printToggle = FALSE, explain = F)

write.csv(table1, file = "characteristics.csv")

table1

}
