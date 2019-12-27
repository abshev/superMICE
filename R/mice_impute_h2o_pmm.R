#' Predictive Mean Matching (PMM) imputation method for \code{mice} using Super Learner via \code{h2o}.
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
#' @param h2o.models A named list of character vectors specifying
#' prediction algorithms and arguments to be passed to h2o.  See details below
#' for examples on the structure.
#' @param donors The size of the donor pool among which a draw is made. The
#' default is donors = 5L. Setting donors = 1L always selects the closest match,
#' but is not recommended. Values between 3L and 10L provide the best results
#' in most cases (Morris et al, 2015).
#' @param ... Further arguments passed to \code{SuperLearner} or \code{h2o}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @details
#' This method is similar to what van Buuren (2018) describes as type 0
#' matching. Distances are determined only from predicted values from a Super
#' Learner model.  The closest \code{k} predictions are used to determine the
#' donor pool.
#'
#' The backend fitting the Super Learner models is H2O.  Models are specified
#' as a list of named character vectors.  The first element of each vector is
#' the name of the function being called in H2O.
#'
#'
#' @example
#' \dontrun{
#' # Define the h2o models
#' h2o.mods = list(TO DO)
#'
#' # Run mice using the h2o normal method
#' imps = mice::mice(mice::nhanes, m = 5, method = "h2o.norm",
#'                    h2o.models = h2o.mods)
#'
#' # list the actual imputations
#' imp$imp$bmi
#'
#' # first completed data matrix
#' complete(imps)
#'
#' # predict hypertensive from age, bmi, and total serum cholesterol
#'
#' }
#'
#' @export
#' @import h2o

mice.impute.h2o.pmm = function(y, ry, x, wy = NULL, h2o.models, donors = 5){
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
    imps = binary.H2O.PMM(Y, X, newdata, SL.library, k = donors)
  }
  else if(class(y) == "numeric"){
    imps = continuous.H2O.PMM(Y, X, newdata, SL.library, k = donors)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
