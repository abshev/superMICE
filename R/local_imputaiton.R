#' Function to generate imputations using regression and SuperLearner for data with a continuous outcome
#'
#' @param x blah
#' @param y blah
#' @param delta blah
#' @param preds blah
#' @param bw blah
#' @param imputation blah
#' @param kernel further arguments passed to SuperLearner
#' @param weightType blah
#' @return nothing
#'



localImputation <- function(i, x, y, delta, bw,
                            imputation = c("semiparametric", "nonparametric"),
                            kernel = c("gaussian"),
                            weightType = c("nadaraya-watson")){
  if(kernel == "gaussian"){
    kernVals = kdgaussian(x = x, lambda = NULL, bw = bw,
                                 kerncentres = x[delta == 1][i])
  }
  if(weightType == "nadaraya-watson"){
    weights = kernVals / sum(kernVals)
  }
  if(imputation == "nonparametric"){
    sample(y[delta == 1], size = 1, prob = weights[delta == 1] /
             sum(weights[delta == 1]))
  }
  else{
    pihat = sum(kernVals * delta) / sum(kernVals)
    muhat = sum(weights * delta * y / pihat)
    sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
    rnorm(1, muhat, sqrt(sig2hat))
  }
}
