#' Function to generate imputations using SuperLearner for data with a binary outcome.
#'
#' @param Y Vector of observed values of the variable to be imputed.
#' @param X Numeric matrix of variables to be used as predictors in SuperLearner methods with rows corresponding to values in Y.
#' @param newdata Numeric matrix of variables to as predictors in SuperLearner methods with rows corresponding to missing values of the variable to be imputed. The SuperLearner model makes predictions from this matrix to determine the imputation-generating distribution.
#' @param SL.library Either a character vector of prediction algorithms or a list containing character vectors. A list of functions included in the SuperLearner package can be found with SuperLearner::listWrappers().
#' @param ... Further arguments passed to SuperLearner.
#' @return Binary Vector of randomly drawn imputed values.
#'
#' @importFrom stats binomial



#Binary SuperLearner regression
binary.SuperLearner = function(Y, X, newdata, SL.library, ...){
  args = c(list(Y = Y, X = X, family = stats::binomial(), SL.library = SL.library),
           list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type = NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])
  # if(method.weights){
  #   .GlobalEnv$superMICE.weights <- c(.GlobalEnv$superMICE.weights,
  #                                     list(sl$coef))
  # }
  p <- predict.SuperLearner(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  binaryImputations = rbinom(length(p), 1, p)
  if(is.factor(Y)){
    levels(Y)[binaryImputations + 1]
  }
  else if(is.logical(Y)){
    c(FALSE, TRUE)[binaryImputations + 1]
  }
  else if(is.numeric(Y) | is.character(Y)){
    sort(Y)[binaryImputations + 1]
  }
  else{
    unique(Y)[binaryImputations + 1]
  }
}
