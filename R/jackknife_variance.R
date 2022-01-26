#' Computes jackknife variance
#'
#' @param j blah
#' @param kernMatrix blah
#' @param n blah
#' @param m blah
#' @param delta blah
#' @param y blah
#' @return variance

jackknifeVariance = function(j, kernMatrix, n, m, delta, y){
  newKernMatrix = kernMatrix[-j,]
  newDelta = delta[-j]
  newy = y[-j]
  newWeight.numerator = newKernMatrix
  newWeight.denominator = colSums(newWeight.numerator)
  newWeightMatrix = newWeight.numerator / matrix(newWeight.denominator,
                                              nrow = n - 1, ncol = m,
                                              byrow = TRUE)
  pihat.jk = colSums(newKernMatrix * newDelta) / colSums(newKernMatrix)
  muhat.jk = colSums(newWeightMatrix * newDelta * newy) / pihat.jk
  mu2hat.jk = colSums(newWeightMatrix * newDelta * newy^2) / pihat.jk
  mu2hat.jk - muhat.jk^2
}
