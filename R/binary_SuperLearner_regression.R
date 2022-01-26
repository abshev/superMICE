#' Function to generate imputations using SuperLearner for data with a binary outcome.
#'
#' @param y Vector of observed values of the variable to be imputed.
#' @param x Numeric matrix of variables to be used as predictors in SuperLearner methods with rows corresponding to values in Y.
#' @param wy blah
#' @param SL.library Either a character vector of prediction algorithms or a list containing character vectors. A list of functions included in the SuperLearner package can be found with SuperLearner::listWrappers().
#' @param ... Further arguments passed to SuperLearner.
#' @return Binary Vector of randomly drawn imputed values.
#'
#' @importFrom stats binomial



#Binary SuperLearner regression
binarySuperLearner = function(y, x, wy, SL.library, ...){
  newdata <- data.frame(x)
  names(newdata) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

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
  phat <- predict.SuperLearner(object = sl, newdata = newdata,
                               X = X, Y = Y, TRUE)$pred
  binaryImputations = stats::rbinom(length(phat[wy]), 1, phat[wy])
  if(is.factor(Y)){
    return(levels(Y)[binaryImputations + 1])
  }
  else if(is.logical(Y)){
    return(c(FALSE, TRUE)[binaryImputations + 1])
  }
  else if(is.numeric(Y) | is.character(Y)){
    return(sort(unique(Y))[binaryImputations + 1])
  }
  else{
    return(unique(Y)[binaryImputations + 1])
  }
}
