#'computes bootstrap coefficients
#' @title coeff.blb
#' @name coeff.blb
#' @param x object
#'
#' @return
#'
#' @export
coeff.blb<-function(x){
  means = data.frame(do.call("rbind", lapply(x$est, "[[", 1)))
  colMeans(means)

}

