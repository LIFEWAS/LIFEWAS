#' The pipe_cox_continuous function
#'
#' This function runs a cox-regression model pipeline for all variables of interest, adjusting for age and sex.
#' The pipeline starts with a cox-regression in group A, after which all variables with a False Discovery Rate (BY) of alpha < 0.05 are selected and the analysis is repeated in group B. Pathways go both ways (A --> B and B --> A)
#' When the variable is significant in none of the pathways, models == "none", when significant in the first but not second model, models == "one", when significant in both pathways models == "two".
#' Output is a dataframe with all variables tested, estimate and p-value from the whole dataset, number of complete cases and significance in pathways
#' This function uses the original factors to improve interpretability. Reference groups have to be set by hand!
#' @param disease name coding for disease variable (e.g. "dmNewDig")
#' @param outDig output file from the digotomous pipeline (pipeCox function), optionally filtered for solely significant variables
#' @param dataframe dataframe with continuous variables (dataCon, created with grouping function)
#' @param key key with workName and outName, for output of proper names
#' @keywords pipeline
#' @examples outDigSig <- outDig %>% filter(models != "none"); outCon <- pipe_cox_continuous(outDigSig, dataCon)
#' pipe_cox_continuous()


pipe_cox_continuous <- function(disease, outDigSig, dataframe, key) {

  sigValues <- outDigSig %>%
    mutate(value = gsub("0.*","0", value)) %>%
    select(value, casesComplete, models) %>%
    unique()

  line <- paste("Surv(time,", disease, ") ~ ")

  coxReg <- function(predictor, dataframe) {
    coxph(as.formula(paste(line, predictor, "+ age0 + sex0")), data = dataframe)
  }

  modelCon <- enframe(sigValues$value, name = NULL) %>%
    mutate(model = (map(sigValues$value, coxReg, dataframe)) %>%
             map(tidy, exponentiate = T)) %>%
    unnest(model) %>%
    filter(term != "Intercept" & term != "age0" & term != "sex0Male") %>%
    mutate(ciLow = round((estimate - 1.96*std.error), 2),
           ciHigh = round((estimate + 1.96*std.error), 2),
           estimate = round(estimate, 2),
           pValue = p.value)

  outCon <- merge(sigValues, modelCon, all.y = T, by = "value") %>%
    select(term, estimate, ciLow, ciHigh, pValue, models, casesComplete) %>%
    arrange(models, pValue)

  outCon2 <- outCon %>%
    separate(term, c("variable", "level"), sep = "0", remove = T, extra = "merge") %>%
    mutate(variable = paste(variable, "0", sep = "")) %>%
    merge(., key, by.x = "variable", by.y = "workName", all.x = T) %>%
    select(outName, level, estimate, ciLow, ciHigh, pValue, models, casesComplete) %>%
    arrange(models, outName) %>%
    distinct(outName, level, .keep_all = TRUE)

  outCon2 %>%
    mutate(hazardRatio95CI = paste(estimate, " (", ciLow, "; ", ciHigh, ")", sep = "")) %>%
    select(outName, level, hazardRatio95CI, pValue, casesComplete, models) %>%
    rename(variable = outName) %>%
    arrange(variable, level) %>%
    write.csv(., "pipeCoxConOut.csv", row.names = F)

  outCon
}
