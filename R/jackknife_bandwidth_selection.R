#' Jackknife method for selection bandwidth
#'
#' @param bwGrid blah
#' @param i blah
#' @param preds blah
#' @param y blah
#' @param delta blah
#' @param lambda blah
#' @param imputation blah
#' @param kernel blah
#' @param weights blah
#' @return bandwidth
#'
#' @importFrom stats dnorm

bandwidth_jackknife_selection = function(bwGrid, i, preds, y, delta,
                                         lambda = NULL, imputation, kernel,
                                         weights){
  if(kernel == "gaussian"){
    kernGrid = lapply(bwGrid, gaussianKernel, x = preds,
                        xcenter = preds[delta == 0][i], lambda = lambda)
  }

  if(kernel == "uniform"){
    kernGrid = lapply(bwGrid, uniformKernel, x = preds,
                        xcenter = preds[delta == 0][i], lambda = lambda)
  }

  if(kernel == "triangular"){
    kernGrid = lapply(bwGrid, triangularKernel, x = preds,
                        xcenter = preds[delta == 0][i], lambda = lambda)
  }
  kernGrid = do.call(cbind, kernGrid)[-i,]
  n = nrow(kernGrid)

  if(weights == "nadaraya-watson"){
    weightGrid = apply(kernGrid, 2, function(x){x / sum(x)})
  }

  if(imputation == "semiparametricSL"){
    pihat.fullData = colSums(kernGrid * delta[-i]) / colSums(kernGrid)
    muhat.fullData = colSums(weightGrid * delta[-i] * y[-i]) / pihat.fullData
    # sig2hat.fullData = colSums(weightGrid * delta * y^2 / pihat.fullData) -
    #   muhat.fullData^2

    muhat.jk = lapply(1:nrow(kernGrid), function(j, kernGrid, weightGrid,
                                                  delta, y){
      pihat.jk = colSums(kernGrid[-j,] * delta[-j]) / colSums(kernGrid[-j,])
      colSums(weightGrid[-j,] * delta[-j] * y[-j]) / pihat.jk
      # colSums(weightGrid[-j,] * delta * y^2 / pihat.jk) - muhat.jk^2
    }, kernGrid = kernGrid, weightGrid = weightGrid, delta = delta[-i],
    y = y[-i])
    # sig2hat.jk = do.call(rbind, sig2hat.jk)
    muhat.jk = do.call(rbind, muhat.jk)

    muhat.jk = ((n^(4 / 5)) * (matrix(1, nrow = n, ncol = 1) %*% muhat.fullData) -
      ((n - 1)^(4 / 5)) * muhat.jk) / (n^(4/5) - (n - 1)^(4/5))

    bias2 = (muhat.fullData - colMeans(muhat.jk))^2
    s2 = colSums((muhat.jk - (matrix(1, nrow = n, ncol = 1) %*%
                                colMeans(muhat.jk)))^2) / (n * (n - 1))
    mse = bias2 + s2

    return(bwGrid[which.min(mse)])

    # bias2 = ((colMeans(sig2hat.jk) - sig2hat.fullData) * (nrow(kernGrid) - 1))^2
    # var = colSums((sig2hat.jk - matrix(1, nrow = nrow(kernGrid), ncol = 1) %*%
    #                  sig2hat.fullData)^2) * (nrow(kernGrid) - 1)



    # rnorm(1, preds[delta == 0][i], sqrt(sig2hat.fullData))
  }




  # nf.div = floor(nrow(weightGrid) / nfolds)
  # nf.rem = nrow(weightGrid) %% nfolds
  # if(nf.rem != 0){
  #   CVindex = sample(c(rep(1:nfolds, times = nf.div), 1:(nf.rem))
  # }
  # else{
  #   CVindex = sample(c(rep(1:nfolds, times = nf.div)))
  # }

  # pihat.cv = vector
  # lapply(1:nfolds, function(i, CVindex, kernGrid){

  # })


  # else if(imputation == "semiparametric"){
  #   pihat = sum(kernVals * delta) / sum(kernVals)
  #   muhat = sum(weights * delta * y / pihat)
  #   sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
  #   rnorm(1, muhat, sqrt(sig2hat))
  # }
  # else if(imputation == "nonparametric"){
  #   sample(y[delta == 1], size = 1, prob = weights[delta == 1] /
  #            sum(weights[delta == 1]))
  # }
#
#   for(bw in bwGrid){
#     imputeFullData = local_imputation(i, preds, y = y, delta, bw = bw, lambda = NULL,
#                                       imputation = c("semiparametric", "nonparametric"),
#                                       kernel = c("gaussian", "uniform", "triangular"),
#                                       weights = c("nadaraya-watson"))
#   }
}
