#' Function to generate imputations using PMM and SuperLearner for data with a continuous outcome
#'
#' @param x blah
#' @param candidates blah
#' @param k blah
#' @return nothing
#'

knnImpute = function(x, candidates, k){
  x.mat = x %*% matrix(1, ncol = length(candidates))
  cand.mat = matrix(candidates, nrow = nrow(x.mat), ncol = length(candidates),
                    byrow = TRUE)
  dist.mat = abs(x.mat - cand.mat)
  randomCandidates = apply(dist.mat, 1, function(dists, cands, k){
    topCands = cands[order(dists)[1:k]]
    topCands[ceiling(runif(1, 0, k))]
  }, cands = candidates, k = k)
  return(randomCandidates)
}
