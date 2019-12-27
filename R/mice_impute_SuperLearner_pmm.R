#' Predictive Mean Matching (PMM) imputation method for \code{mice} using Super Learner via \code{SuperLearner}.
#'
#' Samples imputations for univariate missing data using PMM.
#' Distances for PMM are computed using predictions from a Super Learner model
#' instead of the standard regression model.
#'
#' @param y Vector to be imputed
#' @param ry Logical vector of length length(y) indicating the the subset y[ry]
#' of elements in y to which the imputation model is fitted. The ry generally
#' distinguishes the observed (TRUE) and missing values (FALSE) in y.
#' @param x Numeric design matrix with length(y) rows with predictors for y.
#' Matrix x may have no missing values.
#' @param wy Logical vector of length length(y). A TRUE value indicates
#' locations in y for which imputations are created.
#' @param SL.library A character vector of prediction algorithms or list
#' containing character vectors as specified by the SuperLearner package.
#' @param donors The size of the donor pool among which a draw is made. The
#' default is donors = 5L. Setting donors = 1L always selects the closest match,
#' but is not recommended. Values between 3L and 10L provide the best results
#' in most cases (Morris et al, 2015).
#' @param ... Further arguments passed to \code{SuperLearner}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @export
#' @import SuperLearner
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.SuperLearner.pmm = function(y, ry, x, wy = NULL, SL.library,
                                        donors = 5, ...){
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
