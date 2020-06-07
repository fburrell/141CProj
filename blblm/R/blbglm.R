#'computes bootstraps for glm
#' @title blbglm
#' @name blbglm
#' @import purrr
#' @importFrom stats lm
#' @import rsample
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps


#' @param formula formula
#' @param family family
#' @param data data.frame
#' @param times numeric
#' @return list
#'
#' @example blbglm(Survived~Class+Sex,binomial, data.frame(Titanic),10000)
#'
#' @export
blbglm<-function(formula, family, data, times){
  boots <- bootstraps(data=data , times = times)
  coeffs <- boots %>%
    pull(splits) %>%
    map(
      ~ {
        train_data <- analysis(.)
        list(coeff=with(train_data, glm(formula,family=family, data=train_data)$coefficients),
             sigma=with(train_data, summary(glm(formula, family=family, data=train_data))$coefficients[,2]))

      }
    )
  list(est=coeffs, formula=formula)
}
