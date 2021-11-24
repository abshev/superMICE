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

jackknife.bandwidth.selection = function(i, bwGrid, preds, y, delta,
                                         lambda = NULL, imputation, kernel,
                                         weights){
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

  weight.numerator = kernMatrix# * matrix(delta, nrow = n, ncol = m)
  weight.denominator = colSums(weight.numerator)
  weightMatrix = weight.numerator / matrix(weight.denominator, nrow = n,
                                           ncol = m, byrow = TRUE)
  # if(weights == "nadaraya-watson"){
  #   weightGrid = apply(kernGrid, 2, function(x, delta){
  #     (x * delta) / sum(x * delta)
  #   }, delta = delta)
  # }

  if(imputation == "semiparametricSL"){
    pihat.fullData = colSums(kernMatrix * delta) / colSums(kernMatrix)
    muhat.fullData = colSums(weightMatrix * delta * y) / pihat.fullData
    mu2hat.fullData = colSums(weightMatrix * delta * y^2) / pihat.fullData
    sig2hat.fullData = mu2hat.fullData - muhat.fullData^2
    # sig2hat.fullData = colSums(weightGrid * delta[-i] *
    #                              (matrix(y[-i], ncol = m, nrow = n) -
    #                                 matrix(muhat.fullData, ncol = m, nrow = n,
    #                                        byrow = TRUE))^2)
    # weightGrid2 = weightGrid * delta[-i] / matrix(colSums(weightGrid * delta[-i]),
    #                                               nrow = nrow(weightGrid),
    #                                               ncol = ncol(weightGrid),
    #                                               byrow = TRUE)
    # sig2hat.fullData = ((preds[,1] - preds[i])^2) %*% weightMatrix
    # sig2hat.fullData = (1 / (1 - colSums(weightMatrix^2))) *
    #   ((preds[,1] - preds[i])^2) %*% weightMatrix
    # sig2hat.fullData = apply(weightMatrix, 2, function(w, y){
    #   var(y[w != 0])
    # }, y = y)

    # sig2hat.fullData = diag(t((preds[,1] - matrix((preds[,1] %*% weightMatrix)[1,], nrow = n, ncol = m, byrow = TRUE))^2) %*% weightMatrix)

    # sig2hat.fullData = colSums(weightGrid * delta * y^2 / pihat.fullData) -
    #   muhat.fullData^2
    # kernGridSums = colSums(kernGrid)
    # kernGridSums2 = colSums(kernGrid * delta[-i])
    # weightGridSums = colSums(weightGrid * delta[-i] * y[-i])

    # muhat.jk = lapply((1:n)[delta[-i] == 1], function(j, kernGrid, weightGrid,
    #                                                   delta, y, kernGridSums,
    #                                                   kernGridSums2,
    #                                                   weightGridSums){
    #   pihat.jk = (kernGridSums2 - kernGrid[j,] * delta[j]) /
    #     (kernGridSums - kernGrid[j,])
    #   # pihat.jk = Rfast::colsums(kernGrid[-j,] * delta[-j]) / Rfast::colsums(kernGrid[-j,])
    #   (weightGridSums - weightGrid[j,] * delta[j] * y[j]) / pihat.jk
    #   # Rfast::colsums(weightGrid[-j,] * delta[-j] * y[-j]) / pihat.jk
    #   # colSums(weightGrid[-j,] * delta * y^2 / pihat.jk) - muhat.jk^2
    # }, kernGrid = kernGrid, weightGrid = weightGrid, delta = delta[-i],
    # y = y[-i], kernGridSums = kernGridSums, kernGridSums2 = kernGridSums2,
    # weightGridSums = weightGridSums)
    # sig2hat.jk = do.call(rbind, sig2hat.jk)
    # muhat.jk = do.call(rbind, muhat.jk)

    # sig2hat.jk = lapply((1:n)[delta == 1], function(j, #sig2hat.fullData,
    #                                                 weight.numerator,
    #                                                 weight.denominator,
    #                                                 # delta,
    #                                                 y, m, n, preds, #muhat.jk,
    #                                                 center){
    #   newWeightMatrix = weight.numerator[-j,] /
    #     matrix(weight.denominator - weight.numerator[j,], nrow = n - 1,
    #            ncol = m, byrow = TRUE)
    #
    #
    #   # ((preds[-j] - preds[i])^2) %*% newWeightMatrix
    #   # (1 / (1 - colSums(newWeightMatrix^2))) *
    #   #   ((preds[-j] - preds[i])^2) %*% newWeightMatrix
    #   # apply(newWeightMatrix, 2, function(w, y){
    #   #   var(y[-j][w != 0])
    #   # }, y = y)
    #   # diag(t((preds[-j] - matrix((preds[-j] %*% newWeightMatrix)[1,], nrow = n - 1, ncol = m, byrow = TRUE))^2) %*% newWeightMatrix)
    #   #colSums(newWeights * (y[-j] - center)^2)
    #   # sig2hat.fullData - weightGrid2[j] * (y[j] - center)^2
    #   # colSums(weightGrid2[-j,] * (y[-j] - center)^2)
    #   # sig2hat.fullData - (weightGrid[j,] * delta[j] *
    #   #   (rep(y[j], times = m) - muhat.jk[sum(delta[1:j]),])^2)
    # }, #sig2hat.fullData = sig2hat.fullData, weightGrid = weightGrid,
    # weight.numerator = weight.numerator,
    # weight.denominator = weight.denominator,
    # y = y, preds = preds[,1], center = preds[i], m = m, n = n)
    #delta = delta[-i], y = y[-i], m = m, n = n, muhat.jk = muhat.jk, center = preds[i])

    sig2hat.jk = lapply((1:n)[delta == 1], function(j, kernMatrix, n, m, delta, y){
      newKernMatrix = kernMatrix[-j,]
      newDelta = delta[-j]
      newy = y[-j]
      newWeight.numerator = newKernMatrix
      newWeight.denominator = colSums(newWeight.numerator)
      newWeightMatrix = weight.numerator / matrix(newWeight.denominator,
                                                  nrow = n, ncol = m,
                                                  byrow = TRUE)
      pihat.jk = colSums(newKernMatrix * newDelta) / colSums(newKernMatrix)
      muhat.jk = colSums(newWeightMatrix * newDelta * newy) / pihat.jk
      mu2hat.jk = colSums(newWeightMatrix * delta * newy^2) / pihat.jk
      mu2hat.jk - muhat.jk^2
    }, kernMatrix = kernMatrix, n = n, m = m, delta = delta, y = y)

    sig2hat.jk = do.call(rbind, sig2hat.jk)

    # muhat.jk = ((n^(4 / 5)) * (matrix(1, nrow = n, ncol = 1) %*% muhat.fullData) -
    #   ((n - 1)^(4 / 5)) * muhat.jk) / (n^(4/5) - (n - 1)^(4/5))

    bias2 = (sig2hat.fullData - colMeans(sig2hat.jk))^2
    s2 = colSums((sig2hat.jk - matrix(colMeans(sig2hat.jk),
                                      nrow = nrow(sig2hat.jk), ncol = m,
                                      byrow = TRUE))^2) / (n * (n - 1))
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
