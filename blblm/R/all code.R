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



coeff.blb<-function(x){
  means = data.frame(do.call("rbind", lapply(x$est, "[[", 1)))
  colMeans(means)

}

se.blb<-function(x){
  means = data.frame(do.call("rbind", lapply(x$est, "[[", 2)))
  colMeans(means)

}


confints.blb<-function(object, level=0.95){
  t<-coeff.blb(object)
  s<-se.blb(object)
  alpha<-1-level
  lowerbound<-t(as.data.frame(map(t, function(x)t-qnorm(1-alpha/2)*s)))[1,]
  upperbound<-t(as.data.frame(map(t, function(x)t+qnorm(1-alpha/2)*s)))[1,]
  cbind(lowerbound,upperbound)

}

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
