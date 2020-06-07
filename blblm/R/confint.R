#'computes confidence interval of bootstrap coefficients
#' @title confints.blb
#' @name confints.blb
#' @param object object
#' @param level numeric
#' @return
#'
#' @export
confints.blb<-function(object, level=0.95){
  t<-coeff.blb(object)
  s<-se.blb(object)
  alpha<-1-level
  lowerbound<-t(as.data.frame(map(t, function(x)t-qnorm(1-alpha/2)*s)))[1,]
  upperbound<-t(as.data.frame(map(t, function(x)t+qnorm(1-alpha/2)*s)))[1,]
  cbind(lowerbound,upperbound)

}
