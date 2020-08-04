#' Function to generate imputations using non-parametric and semi-parametric local imputation methods.
#'
#' @param i Integer referring to the index of the missing value to be imputed.
#' @param preds Numeric vector of predictions of missing values from SuperLearner or H2O.
#' @param y Numeric vector for variable to be imputed.
#' @param delta Binary vector of length length(y) with 1 where y is observed and 0 where y is missing.
#' @param bw Bandwidth to be passed to kernel function.
#' @param imputation One of "semiparametric" or "nonparametric". Determines distribution from which imputed values are drawn. See mice.impute.SuperLearner() or mice.impute.h2o() for more details.
#' @param kernel Kernel function used to compute weights.
#' @param weights One of "nadaraya-watson", ...
#' @return Numeric vector of randomly drawn imputed values.
#'



localImputation <- function(i, preds, y, delta, bw,
                            imputation = c("semiparametric", "nonparametric"),
                            kernel = c("gaussian"),
                            weights = c("nadaraya-watson")){
  if(kernel == "gaussian"){
    kernVals = gaussianKernel(x = preds, xcenter = preds[delta == 0][i],
                              bw = bw, lambda = NULL)
  }
  if(weights == "nadaraya-watson"){
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
