#' Kernel functions used for local imputation
#'
#' @param x blah
#' @param xcenter blah
#' @param bw blah
#' @param lambda blah
#' @return nothing
#'

gaussianKernel = function(x, xcenter, bw = 1, lambda = NULL){
  if(is.null(lambda)){
    lambda = bw
  }
  z = (x - xcenter)/lambda
  dnorm(z)/lambda
}
