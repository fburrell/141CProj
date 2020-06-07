#'makes prediction based off bootstrap coefficients
#' @title predicts.blb
#' @name predicts.blb
#' @param object object
#' @param new_data numeric
#' @param level numeric
#' @param confidence boolean
#' @param ... numeric
#'
#'
#' @return
#'

#' @export
predicts.blb <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  object=list(est=list(est=object$est),formula=object$formula)
  est <- object$est
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coeff) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coeff) %>% rowMeans())
  }
}

mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}


map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}


