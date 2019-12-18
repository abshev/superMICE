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
#' @param SL.backend Backend to fit the SuperLearner models.  Must be
#' one of "SuperLearner" or "h2o".
#' @param imputation.method Method used to randomly generate imputed values.
#' Must be one of "Regression" or "PMM".
#' @param donors If PMM imputation method is being used, this is the number
#' of donors from which to draw an imputed value.
#' @param ... Further arguments passed to \code{SuperLearner} or \code{h2o}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @export
#' @import SuperLearner
#' @import h2o
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.SuperLearner.pmm = function(y, ry, x, wy = NULL, SL.library,
                                         SL.CV = TRUE, donors = 5, ...){
  if(!requireNamespace("SuperLearner")){
    stop(simpleError('SuperLearner is not installed.'))
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
    imps = binary.SuperLearner.PMM(Y, X, newdata, SL.library, k = donors, ...)
  }
  else if(class(y) == "numeric"){
    imps = continuous.SuperLearner.PMM(Y, X, newdata, SL.library, k = donors, ...)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
