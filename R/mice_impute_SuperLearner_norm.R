#' SuperLearner method for \code{mice} package.
#'
#' Method for the \code{mice} package that uses SuperLearner as the predctive
#' algorithm.  This is done through a backend powered by either the
#' \code{SuperLearner} package or H2O.
#'
#' @param y Vector to be imputed
#' @param ry Logical vector of length length(y) indicating the the subset y[ry]
#' of elements in y to which the imputation model is fitted. The ry generally
#' distinguishes the observed (TRUE) and missing values (FALSE) in y.
#' @param x Numeric design matrix with length(y) rows with predictors for y.
#' Matrix x may have no missing values.
#' @param wy Logical vector of length length(y). A TRUE value indicates
#' locations in y for which imputations are created.
#' @param SL.library For SuperLearner: Either a character vector of prediction
#' algorithms or list containing character vectors as specified by the
#' SuperLearner package.  For h2o, a named list of character vectors specifying
#' prediction algorithms and arguments to be passed to h2o.  See details below
#' for examples on the structure.
#' @param SL.CV Logical.  If true cv.SuperLearner is used to estimate the risk.
#' @param method.weights If TRUE, for each run of \code{SuperLearner()}, a list
#' of weights for each method is SL.library is returned.
#' @param ... Further arguments passed to \code{SuperLearner} or \code{h2o}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @examples
#'   n <- 1000
#'   pmissing <- 0.10
#'   X1 = runif(n, min = -3, max = 3)
#'   X2 = X1^2 + rnorm(n, mean = 0, sd = 1)
#'   error <- rnorm(n, mean = 0, sd = 1)
#'   Y <- X1 + X2 + error
#'   f <- ecdf(X1)
#'   x2 <- ifelse(runif(X2) < (f(X1) * 2 * pmissing), NA, X2)
#'   x1 <- ifelse(runif(X1) < .2, NA, X1)
#'   y <- ifelse(runif(Y) < .2, NA, Y)
#'   data <- as.data.frame(cbind(y, x1, x2))
#'   SL.lib <- c("SL.glm", "SL.glm.interaction", "SL.mean")
#'   imp.SL <- mice::mice(data, m = 5, method = "SuperLearner.norm",
#'                          print = TRUE, SL.library = SL.lib, SL.CV = TRUE)
#'
#' @export
#' @import SuperLearner
#' @import h2o
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.SuperLearner.norm = function(y, ry, x, wy = NULL, SL.library,
                                         SL.CV = TRUE, method.weights = FALSE,
                                         ...){
  if(!requireNamespace("SuperLearner")){
    stop(simpleError('SuperLearner is not installed.'))
  }

  if(method.weights && is.null(.GlobalEnv$SuperMICE.weights)){
    .GlobalEnv$SuperMICE.weights <- list()
  }

  if (is.null(wy)){
    wy <- !ry
  }
  newdata <- data.frame(x[wy,])
  names(newdata) = sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) = sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

  if(length(unique(y)) == 2){
    imps = binary.SuperLearner.norm(Y, X, newdata, SL.library, SL.CV = SL.CV,
                                    method.weights = method.weights, ...)
  }
  else if(class(y) == "numeric"){
    imps = continuous.SuperLearner.norm(Y, X, newdata, SL.library, SL.CV = SL.CV,
                                        method.weights = method.weights, ...)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
