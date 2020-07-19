#' Function to generate imputations using regression and SuperLearner for data with a continuous outcome
#'
#' @param Y blah
#' @param X blah
#' @param newdata blah
#' @param SL.library blah
#' @param SL.CV blah
#' @param method.weights blah
#' @param ... further arguments passed to SuperLearner
#' @return nothing
#'


#Continuous SuperLearner Regression
continuous.SuperLearner.norm = function(y, x, wy, SL.library, SL.CV,
                                        method.weights, ...){
  newdata <- data.frame(x)
  names(newdata) = sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) = sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

  args = c(list(Y = Y, X = X, family = stats::gaussian(), SL.library = SL.library),
           list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type = NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])
  sl.preds <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  if(method.weights){
    .GlobalEnv$superMICE.weights <- c(.GlobalEnv$superMICE.weights,
                                      list(sl$coef))
  }

  sapply(1:sum(wy), localImputation, x = sl.preds, y = y,
         delta = as.numeric(!wy),
         bw = 0.5,
         imputation = "semiparametric",
         kernel = "gaussian",
         weightType ="nadaraya-watson")
  # if(SL.CV){
  #   cv.sl = do.call(CV.SuperLearner, args)
  #   MSE <- summary(cv.sl)$Table$Ave[1]
  #   sd <- sqrt(MSE * (1 + 1 / nrow(X)))
  # }
  # else{
  #   MSE <- mean((sl$SL.predict - y[!wy])^2)
  #   sd <- sqrt(MSE * (1 + 1 / nrow(X)))
  # }
  # rnorm(length(mu), mu, sd)
}
