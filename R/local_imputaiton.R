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
                            imputation = c("semiparametric.SL", "semiparametric", "nonparametric"),
                            kernel = c("gaussian", "uniform", "triangular"),
                            weights = c("nadaraya-watson")){
  if((is.null(bw) & is.null(lambda))|is.character(bw)){
    if(bw == "type1"){
      difs = abs(preds[delta == 1] - preds[delta == 0][i])
      lambda = difs[order(difs)][ceiling(log(length(difs)))] /
        sd(preds[delta == 1])
    }
    else if(bw == "type2"){
      difs = abs(preds - preds[delta == 0][i])
      lambda = difs[order(difs)][ceiling(log(length(difs)))] /
        sd(preds)
    }
    else if(bw == "type3"){
      difs = abs(preds[delta == 1] - preds[delta == 0][i])
      lambda = quantile(difs, probs = 0.01) /
        sd(preds[delta == 1])
    }
    else if(bw == "type4"){
      difs = abs(preds - preds[delta == 0][i])
      lambda = quantile(difs, probs = 0.01) /
        sd(preds)
    }
    else if(bw == "type5"){
      difs = abs(preds[delta == 1] - preds[delta == 0][i])
      lambda = difs[order(difs)][10] /
        sd(preds[delta == 1])
    }
    else if(bw == "type6"){
      difs = abs(preds - preds[delta == 0][i])
      lambda = difs[order(difs)][10] /
        sd(preds)
    }
  }

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
  if(imputation == "nonparametric"){
    sample(y[delta == 1], size = 1, prob = weights[delta == 1] /
             sum(weights[delta == 1]))
  }
  else if(imputation == "semiparametric"){
    pihat = sum(kernVals * delta) / sum(kernVals)
    muhat = sum(weights * delta * y / pihat)
    sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
    rnorm(1, muhat, sqrt(sig2hat))
  }
  else if(imputation == "semiparametric.SL"){
    pihat = sum(kernVals * delta) / sum(kernVals)
    muhat = sum(weights * delta * y / pihat)
    sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
    rnorm(1, preds[delta == 0][i], sqrt(sig2hat))
  }
}
