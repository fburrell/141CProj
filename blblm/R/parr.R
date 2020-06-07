#'computes bootstraps with parallel computing
#' @title parr.blblm
#' @name parr.blblm
#' @import parallel
#' @import purrr
#' @import dplyr
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
#' @param formula formula
#' @param data data.frame
#' @param B numeric
#' @param cores numeric
#'
#' @return list
#' @export
#'
#' @examples parr.blblm(wt~mpg,mtcars,1000,2)
parr.blblm <- function(formula,data, B, cores){
  cl<-parallel::makeCluster(cores)
  data<-data
  rs <- parallel::parSapply(cl, seq_len(B), function(i) {
    model<-dplyr::sample_n(data, nrow(data), replace = TRUE)
    list(list(coeff=lm(formula, data=model)$coefficients,
              sigma=summary(lm(formula=formula, data=model))$coefficients[,2]))


  })
  parallel::stopCluster(cl)
  list(est=rs, formula=formula)}
