#' Kernel functions used for local imputation
#'
#' @param x blah
#' @param xcenter blah
#' @param bw blah
#' @param lambda blah
#' @return nothing
#'
#' @importFrom stats dnorm

gaussianKernel = function(x, xcenter, bw = 1, lambda = NULL){
  if(is.null(lambda)){
    lambda = bw
  }
  z = (x - xcenter)/lambda
  dnorm(z)/lambda
}

uniformKernel = function(x, xcenter, bw = 1, lambda = NULL){
  if(is.null(lambda)){
    lambda = bw / sqrt(1 / 3)
  }
  z = (x - xcenter)/lambda
  ((abs(z) <= 1) / lambda) * (1 / 2)
}

triangularKernel = function(x, xcenter, bw = 1, lambda = NULL){
  if(is.null(lambda)){
    lambda = bw / sqrt(1 / 6)
  }
  z = (x - xcenter)/lambda
  pmax(1 - abs(z), 0)/lambda
}
