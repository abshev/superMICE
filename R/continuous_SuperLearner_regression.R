#Continuous SuperLearner Regression
continuous.SuperLearner.regression = function(Y, X, newdata, SL.library, SL.CV,
                                              ...){
  args = c(list(Y = Y, X = X, family = gaussian(), SL.library = SL.library),
           list(...))
  args$type = NULL
  # namesArgs = names(args)
  # namesSL = names(formals(SuperLearner))
  # formalsSL = formals(SuperLearner)
  # formalsSL[intersect(namesArgs, namesSL)] <- args[intersect(namesArgs, namesSL)]
  sl <- do.call(SuperLearner, args)
  # sl <- SuperLearner(Y = Y, X = X, family = gaussian(),
  #                   SL.library = SL.library, ...)
  mu <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  if(SL.CV){
    # namesCVSL = names(formals(CV.SuperLearner))
    # formals(CV.SuperLearner)[intersect(namesArgs, namesCVSL)] <-
    # args[intersect(namesArgs, namesCVSL)]
    # cv.sl = CV.SuperLearner(Y = Y, X = X, family = gaussian(),
    # SL.library = SL.library)
    cv.sl = do.call(CV.SuperLearner, args)
    MSE <- summary(cv.sl)$Table$Ave[1]
    sd <- sqrt(MSE * (1 + 1 / nrow(X)))
  }
  else{
    MSE <- mean((sl$SL.predict - y[!wy])^2)
    sd <- sqrt(MSE * (1 + 1 / nrow(X)))
  }
  rnorm(length(mu), mu, sd)
}
