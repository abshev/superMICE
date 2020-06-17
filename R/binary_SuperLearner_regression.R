#' Function to generate imputations using regression and SuperLearner for data with a binary outcome
#'
#' @param Y blah
#' @param X blah
#' @param newdata blah
#' @param SL.library blah
#' @param SL.CV blah
#' @param ... further arguments passed to SuperLearner
#' @return nothing
#'



#Binary SuperLearner regression
binary.SuperLearner.norm = function(Y, X, newdata, SL.library, SL.CV,
                                    method.weights, ...){
  args = c(list(Y = Y, X = X, family = stats::binomial(), SL.library = SL.library),
           list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type = NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])
  if(method.weights){
    .GlobalEnv$superMICE.weights <- c(.GlobalEnv$superMICE.weights,
                                      list(sl$coef))
  }
  p <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
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
