#'computes bootstrap se
#' @title se.blb
#' @name se.blb
#' @param x object
#'
#' @return
#'
#' @export
se.blb<-function(x){
  means = data.frame(do.call("rbind", lapply(x$est, "[[", 2)))
  colMeans(means)

}
