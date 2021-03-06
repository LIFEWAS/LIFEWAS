#' The pipeCox function
#'
#' This function runs a cox-regression model pipeline for all variables of interest, adjusting for age and sex.
#' The pipeline starts with a cox-regression in group A, after which all variables with a False Discovery Rate (BY) of alpha < 0.05 are selected and the analysis is repeated in group B. Pathways go both ways (A --> B and B --> A)
#' When the variable is significant in none of the pathways, models == "none", when significant in the first but not second model, models == "one", when significant in both pathways models == "two".
#' Output is a dataframe with all variables tested, estimate and p-value from the whole dataset, number of complete cases and significance in pathways
#' @param disease name coding for disease variable (e.g. "dmNewDig")
#' @param exclude Variables which are not of interest
#' @param group Variable which identifies respective group (created with grouping function)
#' @param dataframe dataframe (dataDig, created with grouping function)
#' @keywords pipeline
#' @examples outDig <- pipe_cox("dmNewDig", exclude, group, dataDig)
#' pipe_cox()


pipe_cox <- function(disease, exclude, group, dataframe) {

  line <- paste("Surv(time,", disease, ") ~ ")

  cox_reg <- function(predictor, dataframe) {
    coxph(as.formula(paste(line, predictor, "+ age0 + sex0")), data = dataframe)
  }

  imput <- dataframe %>% select(-c(exclude)) %>% names()

  a <- filter(dataframe, group == "A")
  b <- filter(dataframe, group == "B")

  outAtoB <- enframe(imput, name = NULL) %>%
    mutate(model = (map(imput, cox_reg, a)) %>%
             map(tidy)) %>%
    unnest(model) %>%
    filter(term != "(Intercept)" & term != "age0" & term != "sex0Male") %>%
    mutate(FDR = p.adjust(p.value, method = "BY")) %>%
    filter(FDR < 0.05) %>%
    select(value) %>%
    mutate(model = (map(unlist(., use.names = F), cox_reg, b)) %>%
             map(tidy, exponentiate = T)) %>%
    unnest(model) %>%
    filter(term != "(Intercept)" & term != "age0" & term != "sex0Male") %>%
    mutate(ciLow = round((estimate - 1.96*std.error), 2),
           ciHigh = round((estimate + 1.96*std.error), 2),
           ci = paste(ciLow, "; ", ciHigh, sep = "")) %>%
    select(value, term, estimate, ci, p.value) %>%
    filter(p.value < 0.05) %>%
    rename(estimateAtoB = estimate, ciAtoB = ci, p.valueAtoB = p.value)

  outBtoA <- enframe(imput, name = NULL) %>%
    mutate(model = (map(imput, cox_reg, b)) %>%
             map(tidy)) %>%
    unnest(model) %>%
    filter(term != "(Intercept)" & term != "age0" & term != "sex0Male") %>%
    mutate(FDR = p.adjust(p.value, method = "BY")) %>%
    filter(FDR < 0.05) %>%
    select(value) %>%
    mutate(model = (map(unlist(., use.names = F), cox_reg, a)) %>%
             map(tidy, exponentiate = T)) %>%
    unnest(model) %>%
    filter(term != "(Intercept)" & term != "age0" & term != "sex0Male") %>%
    mutate(ciLow = round((estimate - 1.96*std.error), 2),
           ciHigh = round((estimate + 1.96*std.error), 2),
           ci = paste(ciLow, "; ", ciHigh, sep = "")) %>%
    select(value, term, estimate, ci, p.value) %>%
    filter(p.value < 0.05) %>%
    rename(estimateBtoA = estimate, ciBtoA = ci, p.valueBtoA = p.value) %>%
    merge(outAtoB, by = c("term", "value"))

  duo <- outBtoA %>%
    select(-term)

  uno <- anti_join(outAtoB, outBtoA)

  both <- uno %>%
    select(-term) %>%
    merge(., duo, all=T) %>%
    mutate(models = case_when(
      is.na(estimateBtoA) | is.na(estimateAtoB) ~ "one",
      !is.na(estimateBtoA) & !is.na(estimateAtoB) ~ "two")) %>%
    select(value, models)

  effect <- enframe(imput, name = NULL) %>%
    mutate(model = (map(imput, cox_reg, dataframe)) %>%
             map(tidy, exponentiate = T)) %>%
    unnest(model) %>%
    filter( term != "age0" & term != "sex0Male") %>%
    mutate(ciLow = round((estimate - 1.96*std.error), 2),
           ciHigh = round((estimate + 1.96*std.error), 2),
           estimate = round(estimate, 2),
           pValue = p.value) %>%
    select(value, estimate, ciLow, ciHigh, pValue)

  n1 <- dataframe %>% select(unlist(effect$value, use.names = F))

  final <- effect %>%
    mutate(noofna = colSums(is.na(n1))) %>%
    mutate(casesComplete = nrow(n1) - noofna) %>%
    select(-noofna) %>%
    merge(., both, by = "value", all.x = T) %>%
    mutate(models = replace_na(models, "none")) %>%
    mutate(models = fct_relevel(models, "two", "one", "none")) %>%
    arrange(models, pValue)

  final %>%
    mutate(
      variable = gsub("0","", value),
      hazardRatio95CI = paste(estimate, " (", ciLow, "; ", ciHigh, ")", sep = "")) %>%
    select(variable, hazardRatio95CI, pValue, casesComplete, models) %>%
    write.csv(., "pipeCoxDigOut.csv", row.names = F)

  final
}

