#' Jackknife method for selection bandwidth
#'
#' @param bwGrid blah
#' @param i blah
#' @param preds blah
#' @param y blah
#' @param delta blah
#' @param lambda blah
#' @param kernel blah
#' @return bandwidth

jackknifeBandwidthSelection = function(i, bwGrid, preds, y, delta,
                                         lambda = NULL, kernel){
  if(kernel == "gaussian"){
    kernGrid = lapply(bwGrid, gaussianKernel, x = preds,
                        xcenter = preds[i], lambda = lambda)
  }

  if(kernel == "uniform"){
    kernGrid = lapply(bwGrid, uniformKernel, x = preds,
                        xcenter = preds[i], lambda = lambda)
  }

  if(kernel == "triangular"){
    kernGrid = lapply(bwGrid, triangularKernel, x = preds,
                        xcenter = preds[i], lambda = lambda)
  }
  kernMatrix = do.call(cbind, kernGrid)
  n = nrow(kernMatrix)
  m = length(bwGrid)

  weight.numerator = kernMatrix
  weight.denominator = colSums(weight.numerator)
  weightMatrix = weight.numerator / matrix(weight.denominator, nrow = n,
                                           ncol = m, byrow = TRUE)

  pihat.fullData = colSums(kernMatrix * delta) / colSums(kernMatrix)
  muhat.fullData = colSums(weightMatrix * delta * y) / pihat.fullData
  mu2hat.fullData = colSums(weightMatrix * delta * y^2) / pihat.fullData
  sig2hat.fullData = mu2hat.fullData - muhat.fullData^2

  sig2hat.jk = lapply((1:n)[delta == 1], jackknifeVariance,
                      kernMatrix = kernMatrix, n = n, m = m,
                      delta = delta, y = y)

  sig2hat.jk = do.call(rbind, sig2hat.jk)

  bias2 = (sig2hat.fullData - colMeans(sig2hat.jk))^2
  s2 = colSums((sig2hat.jk - matrix(colMeans(sig2hat.jk),
                                    nrow = nrow(sig2hat.jk), ncol = m,
                                    byrow = TRUE))^2) / (n * (n - 1))
  mse = bias2 + s2

  return(bwGrid[which.min(mse)])
}
