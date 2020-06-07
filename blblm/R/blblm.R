#'computes bootstraps
#' @title blblm
#' @name blblm
#' @import purrr
#' @import rsample
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @details
#' Linear Regression with Little Bag of Bootstraps


#' @param formula formula
#'
#' @param data data.frame
#' @param times numeric
#' @example blblm(wt~mpg,mtcars,1000)
#' @return list
#'
#' @export
blblm<-function(formula, data, times){
  boots <- rsample::bootstraps(data=data , times = times)
  coeffs <- boots %>%
    pull(splits) %>%
    map(
      ~ {
        train_data <- analysis(.)
        list(coeff=with(train_data, lm(formula, data=train_data)$coefficients),
             sigma=with(train_data, summary(lm(formula, data=train_data))$coefficients[,2]))

      }
    )
  list(est=coeffs, formula=formula)
}

