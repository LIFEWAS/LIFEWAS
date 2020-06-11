#' The lassoing function
#'
#' This function performs lasso regression on the different pre-defined groups. 
#' All variables of the respective group are included, after which Lasso regression is performed to perform variable selection. 
#' A 10-fold cross-validation is used. The final model is printed as "name"Lasso.csv.
#' The output of this function can be used to subsequently plot the ROC curves.
#' @param variables A dataframe with a column for all variables of interest and their respective groups
#' @param subgroup The group which includes all variables of interest for that group
#' @param number Number of observation a variable needs in order to be included. Some variables with many missing may reduce the sample size of the total analysis and may be considered to exclude. Defaults to n=1.
#' @param name The name of the output file. Defaults to name of subgroup (warning: when including multiple subgroups, the default leads to error)
#' @param l The way lambda should be picked (i.e. "lamda.min" or "lambda.1se")
#' @param dataframe Dataframe
#' @keywords group
#' @examples anthro <- lassoing(dfVulcanoTwo, "Anthropometrics", dataCon)
#' lassoing()

lassoing <- function(variables, subgroup, dataframe, number = 1, l = "lambda.1se",  name = subgroup) {
  set.seed(4321)

  vals <- variables %>%
    mutate(term = gsub("0.*", "0", term)) %>%
    filter(group2 %in% subgroup) %>%
    filter(casesComplete > number) %>%
    select(term)
  
  dfGroup <- dataframe %>%
    select("dmNewDig", "time", "age0", "sex0", vals$term) %>%
    na.omit()
  
  m1x <- model.matrix( ~ . -1 - time - dmNewDig, dfGroup)
  m1y <- Surv(dfGroup$time, 1-(dfGroup$dmNewDig-1))
  m1bestL <- cv.glmnet(m1x, m1y,  family = "cox", alpha = 1, nfolds=10)
  m1e <- glmnet(m1x, m1y,  family="cox", lambda = m1bestL[[l]], alpha = 1)
  m1f <- coxph(as.formula(paste("Surv(time, dmNewDig) ~", paste0(gsub("0.*","", names(as.matrix(coef(m1e))[as.matrix(coef(m1e))[,1] != 0,])), "0", collapse = " + "))), data = dataframe)
  m1g <- tidy(m1f, exponentiate = T) %>% select(term, estimate, conf.low, conf.high, p.value)  %>%
    mutate_at(vars(estimate, conf.low, conf.high), funs(round(., 2))) %>% mutate(cIndex = m1f$concordance[6]) %>%
    separate(term, c("variable", "level"), sep = "0", remove = T, extra = "merge") %>%
    mutate(variable = paste(variable, "0", sep = "")) %>%
    merge(., key, by.x = "variable", by.y = "workName", all.x = T) %>%
    mutate(hazardRatio95CI = paste(estimate, " (", conf.low, "; ", conf.high, ")", sep = "")) %>%
    select(outName, level, hazardRatio95CI, p.value, cIndex) %>%
    rename(variable = outName, pValue = p.value) %>%
    arrange(variable, level) %>%
    write.csv(., paste(name, "Lasso.csv", sep = ""), row.names = FALSE)
  
  data1 <- dataframe %>% select(time, dmNewDig, gsub("0.*", "0", (names(m1f$coefficients)))) %>% na.omit()
  tot2 <- roc(data1$dmNewDig, m1f$linear.predictors)
}


sePlot <- function(outputLasso){
  ci <- ci.se(outputLasso, specificities = seq(0, 1, l = 25))
  ci2 <- data.frame(x = as.numeric(rownames(ci)),
                    lowerTot = ci[,1],
                    upperTot = ci[,2])
  ci2
}
