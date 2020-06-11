#' The dividing function
#'
#' This function creates the variable group, which divides the dataframe into two different groups; Group A and Group B.
#' @param split Variable on which to randomly split the dataframe. put in parenteces ("")
#' @param marge Amount of variation between groups (1 = equal groups). Smaller marge may lead to longer runtime
#' @param id Unique subject ID
#' @param dataframe Dataframe
#' @keywords divide, group
#' @examples dataGrouped <- dividing("zip", 1, "PSEUDOIDEXT", data)
#' dividing()

dividing <- function(split, marge, id, dataframe){
  #Setting seed for reproducibility
  set.seed(4321)

  #Creating variables
  splitct <- 0
  index <- 0

  #Looping until required margins of both groups are met, then outputting required dataframe
  while(!(splitct > ((nrow(dataframe)/2)-marge) & splitct < ((nrow(dataframe)/2)+marge))){
    in1 <- unique(dataframe[,split]) %>% table %>% names
    tmp1 <- sample(in1, length(in1)/2, replace = FALSE)
    index <- which(dataframe[,split] %in% tmp1)
    splitct <- length(index)
    print(splitct)
  }

  #ID's of group A in a, group B in b
  a <- dataframe[index, id]
  b <- dataframe[-index, id]

  #Creating new variable "group" and assigning either 0 or 1 to the specific group
  dataframe <- dataframe %>%
    mutate(group = case_when(
      .[,id] %in% a ~ "A" ,
      .[,id] %in% b  ~ "B" )) %>%
    select(-split)
}
