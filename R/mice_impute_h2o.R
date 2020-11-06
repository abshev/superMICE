#' Normal model imputation method for \code{mice} parameterized by Super Learner via \code{h2o}.
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
#' @param h2o.models A named list of character vectors specifying
#' prediction algorithms and arguments to be passed to h2o.  See details below
#' for examples on the structure.
#' @param ... Further arguments passed to \code{SuperLearner} or \code{h2o}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @details
#' TO DO: Add details
#'
#' @examples
#' \dontrun{
#' # Define the h2o models
#' h2o.mods = list()
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
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.h2o.norm = function(y, ry, x, wy = NULL, h2o.models,
                                kernel = c("gaussian", "uniform",
                                           "triangular"),
                                bw = NULL, lambda = NULL,
                                imputation = c("semiparametricSL",
                                               "semiparametric",
                                               "nonparametric"),
                                weights = "nadaraya-watson",
                                return.method.weights = TRUE, ...){
  if(!requireNamespace("h2o")){
    stop(simpleError('h2o is not installed.'))
  }

  # if(method.weights && is.null(.GlobalEnv$SuperMICE.weights)){
  #   .GlobalEnv$SuperMICE.weights <- list()
  # }

  if (is.null(wy)){
    wy <- !ry
  }

  if(length(unique(y)) == 2){
    imps = binary.H2O(y, x, wy, SL.library, ...)
  }
  else if(class(y) == "numeric"){
    imps = continuous.H2O(y, x, wy, SL.library, kernel = kernel,
                                   bw = bw, lambda = lambda,
                                   imputation = imputation,
                                   weights = weights, ...)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)}
