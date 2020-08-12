#' Function to generate imputations using regression and SuperLearner for data with a continuous outcome
#'
#' @param y Vector of observed values of the variable to be imputed.
#' @param x Numeric matrix of variables to be used as predictors in H2O methods
#' with rows corresponding to observed values of the variable to be imputed.
#' @param wy Logical vector of length length(y). A TRUE value indicates
#' locations in y for which imputations are created.
#' @param SL.library Either a character vector of prediction algorithms or a
#' list containing character vectors. A list of functions included in the
#' SuperLearner package can be found with SuperLearner::listWrappers().
#' @param bw NULL or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param lambda NULL or numeric value for bandwidth for kernel (as half-width of the kernel).
#' @param imputation One of "semiparametric" or "nonparametric". Determines
#' distribution from which imputed values are drawn. See
#' mice.impute.SuperLearner() documentation for more details.
#' @param kernel One of "gaussian",...  Kernel function used to compute weights.
#' @param weights One of "nadaraya-watson", ...
#' @param ... further arguments passed to SuperLearner.
#' @return Numeric vector of randomly drawn imputed values.
#'


#Continuous SuperLearner Regression
continuous.SuperLearner <- function(y, x, wy, SL.library, kernel, bw, lambda,
                                    imputation, weights, ...){
  newdata <- data.frame(x)
  names(newdata) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

  args <- c(list(Y = Y, X = X, family = stats::gaussian(),
                 SL.library = SL.library),
            list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type <- NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])
  sl.preds <- predict.SuperLearner(object = sl, newdata = newdata, X = X, Y = Y,
                                   TRUE)$pred
  # if(method.weights){
  #   .GlobalEnv$superMICE.weights <- c(.GlobalEnv$superMICE.weights,
  #                                     list(sl$coef))
  # }

  sapply(1:sum(wy), localImputation, preds = sl.preds, y = y,
         delta = as.numeric(!wy),
         bw = bw, lambda = lambda,
         imputation = "semiparametric",
         kernel = kernel,
         weights ="nadaraya-watson")
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
