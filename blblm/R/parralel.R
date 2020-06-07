#'computes bootstraps with parallel computing
#' @title parr.blbglm
#' @name parr.blbglm
#' @import parallel
#' @import purrr
#' @import dplyr
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
#' @param formula formula
#' @param family, family
#' @param data data.frame
#' @param B numeric
#' @param cores numeric
#'
#' @return list
#' @export
#'
#' @examples parr.blbglm(Survived~Class+Sex,binomial, data.frame(Titanic),100,2)
parr.blbglm <- function(formula,family,data, B, cores){
  cl<-makeCluster(cores)
  data<-data
  rs <- parSapply(cl, seq_len(B), function(i) {
    model<-dplyr::sample_n(data, nrow(data), replace = TRUE)
    list(list(coeff=glm(formula,family=family, data=model)$coefficients,
              sigma=summary(glm(formula=formula, family=family, data=model))$coefficients[,2]))


  })
  stopCluster(cl)
  list(est=rs, formula=formula)}
