#Binary SuperLearner regression
binary.SuperLearner.regression = function(Y, X, newdata, SL.library, SL.CV,
                                          ...){
  args = c(list(Y = Y, X = X, family = binomial(), SL.library = SL.library),
           list(...))
  args$type = NULL
  sl <- do.call(SuperLearner, args)
  # sl <- SuperLearner(Y = Y, X = X, family = binomial(),
  #                   SL.library = SL.library, ...)
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
