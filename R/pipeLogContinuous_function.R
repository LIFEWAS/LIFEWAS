#' The pipe_log function for continuous traits
#'

pipe_log_continuous <- function(disease, outDigSig, dataframe, key) {

  sigValues <- outDigSig %>%
    mutate(value = gsub("0.*","0", value)) %>%
    select(value, casesComplete, models) %>%
    unique()

  line <- paste(disease, " ~ ")

  log_reg <- function(predictor, dataframe) {
    glm(as.formula(paste(line, predictor, "+ age0 + sex0")), family = "binomial", data = dataframe)
  }

  modelCon <- enframe(sigValues$value, name = NULL) %>%
    mutate(model = (map(sigValues$value, log_reg, dataframe)) %>%
             map(tidy, exponentiate = T)) %>%
    unnest(model) %>%
    filter(term != "(Intercept)" & term != "age0" & term != "sex0Male") %>%
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
    write.csv(., "pipeLogConOut.csv", row.names = F)

  outCon
}
