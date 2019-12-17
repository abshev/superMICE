#' Method for \code{mice} using the normal SuperLearner model fit
#' using \code{h2o}.
#'
#' Calculates imputations for univariate missing data by the normal model.
#' Parameters for the normal distribution generating the data are determined
#' using a SuperLearner model fit in H2O.
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

mice.impute.h2o.norm = function(y, ry, x, wy = NULL, h2o.models){
  method <- match.arg(imputation.method)
  if(!requireNamespace("h2o")){
    stop(simpleError('h2o is not installed.'))
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
    imps = binary.H2O.norm(Y, X, newdata, SL.library)
  }
  else if(class(y) == "numeric"){
    imps = continuous.H2O.norm(Y, X, newdata, SL.library)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
