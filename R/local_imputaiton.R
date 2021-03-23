#' Function to generate imputations using non-parametric and semi-parametric local imputation methods.
#'
#' @param i Integer referring to the index of the missing value to be imputed.
#' @param preds Numeric vector of predictions of missing values from SuperLearner or H2O.
#' @param y Numeric vector for variable to be imputed.
#' @param delta Binary vector of length length(y) with 1 where y is observed and 0 where y is missing.
#' @param bw NULL or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param lambda NULL or numeric value for bandwidth for kernel (as half-width of the kernel).
#' @param imputation One of "semiparametric" or "nonparametric". Determines distribution from which imputed values are drawn. See mice.impute.SuperLearner() or mice.impute.h2o() for more details.
#' @param kernel Kernel function used to compute weights.
#' @param weights One of "nadaraya-watson", ...
#' @return Numeric vector of randomly drawn imputed values.
#'
#' @importFrom stats rnorm



localImputation <- function(i, preds, y, delta, bw = NULL, lambda = NULL,
                            imputation = c("semiparametricSL", "semiparametric",
                                           "nonparametric"),
                            kernel = c("gaussian", "uniform", "triangular"),
                            weights = c("nadaraya-watson")){
  # if(is.null(bw) & is.null(lambda)){
  #   difs = abs(preds - preds[delta == 0][i])
  #   lambda = min(difs[order(difs)][ceiling(log(length(difs)))] /
  #     sd(preds),
  #     difs[order(difs)][ceiling(length(difs) * 0.01)] /
  #       sd(preds))
  # }

  bw = bandwidth_jackknife_selection(bwGrid = bw, i = i, preds = preds, y = y,
                                delta = delta, lambda = lambda,
                                imputation = imputation, kernel = kernel,
                                weights = weights)

  if(kernel == "gaussian"){
    kernVals = gaussianKernel(x = preds, xcenter = preds[delta == 0][i],
                              bw = bw, lambda = lambda)
  }
  else if(kernel == "uniform"){
    kernVals = uniformKernel(x = preds, xcenter = preds[delta == 0][i],
                             bw = bw, lambda = lambda)
  }
  else if(kernel == "triangular"){
    kernVals = triangularKernel(x = preds, xcenter = preds[delta == 0][i],
                                bw = bw, lambda = lambda)
  }
  if(weights == "nadaraya-watson"){
    weights = kernVals / sum(kernVals)
  }
  # if(weights == "biasedBootstrapWeights"){
  #
  # }
  if(imputation == "semiparametricSL"){
    pihat = sum(kernVals * delta) / sum(kernVals)
    muhat = sum(weights * delta * y / pihat)
    sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
    rnorm(1, preds[delta == 0][i], sqrt(sig2hat))
  }
  else if(imputation == "semiparametric"){
    pihat = sum(kernVals * delta) / sum(kernVals)
    muhat = sum(weights * delta * y / pihat)
    sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
    rnorm(1, muhat, sqrt(sig2hat))
  }
  else if(imputation == "nonparametric"){
    sample(y[delta == 1], size = 1, prob = weights[delta == 1] /
             sum(weights[delta == 1]))
  }
}
