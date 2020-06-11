#' The bootstrap_cox function
#'

bootstrap_cox_function <- function(variables, number, model_data, ndraws, l = "lambda.1se") {
  #create empty dataframe with names of all variables with >60,000 complete cases
  set.seed(4321)

  vals <- dfVulcanoTwo %>%
    mutate(term = gsub("0.*", "0", term)) %>%
    filter(term %in% variables) %>%
    filter(casesComplete > number) %>%
    select(term)

  extract_names <- coxph(as.formula(paste("Surv(time, dmNewDig) ~", paste0(gsub("0.*","", vals[,1]), "0", collapse = " + "))), dataCon) %>%
    tidy()

  extract_names_2 <- extract_names[,1]

  coef_mtx <- extract_names_2

  for (i in 1:ndraws) {

    #Drawing out a random subset of id's
    #Because of replace = TRUE --> one case can be selected mulitple times
    bootstrap_ids <- sample(seq(nrow(model_data)),
                            nrow(model_data),
                            replace = TRUE)

    #Selecting the generated id's from the full dataset
    bootstrap_data <- model_data[bootstrap_ids,]

    #select all complete cases with variables >60,000 cases in full dataset
    dfGroup <- bootstrap_data %>%
      dplyr::select("dmNewDig", "time", "age0", "sex0", vals$term) %>%
      na.omit()

    #Selecting lambda (lambda.1se for leniency)
    m1x <- model.matrix( ~ . -1 - time - dmNewDig, dfGroup)
    m1y <- Surv(dfGroup$time, 1-(dfGroup$dmNewDig-1))
    m1bestL <- cv.glmnet(m1x, m1y,  family = "cox", alpha = 1, nfolds=10)
    m1e <- glmnet(m1x, m1y,  family="cox", lambda = m1bestL[[l]], alpha = 1)

    #Running model with "best" lambda
    bootstrap_model <- coxph(as.formula(paste("Surv(time, dmNewDig) ~", paste0(gsub("0.*","", names(as.matrix(coef(m1e))[as.matrix(coef(m1e))[,1] != 0,])), "0", collapse = " + "))), bootstrap_data)

    model_tidy <- tidy(bootstrap_model, exponentiate = T) %>%
      dplyr::select(term, p.value) %>%
      rename(i = p.value)

    #Storing p-values in dataframe
    coef_mtx <- coef_mtx %>% merge(., model_tidy, by = "term", all = T)

  }

  trans <- function(x){
    x %>%
      mutate_at(.vars = vars(contains("model")),
                .funs = funs(as.numeric(case_when(
                  . < 0.05 ~ "1",
                  . >= 0.05 ~ "0",
                  TRUE ~ NA_character_
                )))) %>%
      mutate(total = select(., contains("model")) %>%  rowSums(na.rm = TRUE)) %>%
      dplyr::select(term, total) %>%
      arrange(desc(total))
  }


  colnames(coef_mtx) <- c("term", paste("model", seq(1, ndraws, by = 1), sep = ""))

  out <- trans(coef_mtx)

  return(out)
}
