#' Function to generate imputations using PMM and SuperLearner for data with a binary outcome
#'
#' @param Y blah
#' @param X blah
#' @param newdata blah
#' @param SL.library blah
#' @param k blah
#' @param ... further arguments passed to SuperLearner
#' @return nothing
#'



#Binary SuperLearner PMM
binary.SuperLearner.PMM = function(Y, X, wy, SL.library, k, ...){
  args = c(list(Y = Y, X = X, family = stats::binomial(), SL.library = SL.library),
           list(...))
  args$type = NULL
  sl <- do.call(SuperLearner, args)
  pred.impute <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  pred.candidates <- predict(object = sl, newdata = X, X = X, Y = Y, TRUE)$pred
  knnImpute(pred.impute, pred.candidates, k)
}
