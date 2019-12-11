#' Function to generate imputations using PMM and SuperLearner for data with a continuous outcome
#'
#' @param Y blah
#' @param X blah
#' @param newdata blah
#' @param SL.library blah
#' @param k blah
#' @param ... further arguments passed to SuperLearner
#' @return nothing
#'



continuous.SuperLearner.PMM = function(Y, X, newdata, SL.library, k, ...){
  args = c(list(Y = Y, X = X, family = stats::gaussian(), SL.library = SL.library),
           list(...))
  args$type = NULL
  sl <- do.call(SuperLearner, args)
  # sl <- SuperLearner(Y = Y, X = X, family = gaussian(),
  #                   SL.library = SL.library, ...)
  mu <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  candidates <- FNN::get.knn(matrix(mu, ncol = 1), k = k)
  randomCandidates <- apply(candidates$nn.index, 1,
                            function(x, k){x[sample(1:k, 1)]}, k = k)
  Y[randomCandidates]
}
