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
#' @param kernel One of "gaussian", "uniform", "triangular".  Kernel function
#' used to compute weights.
#' @param bw NULL or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param bw.update jefeaerg
#' @param lambda NULL or numeric value for bandwidth for kernel (as half-width of the kernel).
#' @param imputation One of "semiparametric" or "nonparametric". Determines
#' distribution from which imputed values are drawn. See
#' mice.impute.SuperLearner() documentation for more details.
#' @param weights One of "nadaraya-watson", ...
#' @param ... Further arguments passed to \code{SuperLearner} or \code{h2o}.
#' @return Vector with imputed data, same type as y, and of length sum(wy)
#'
#' @examples
#'   n <- 100
#'   pmissing <- 0.10
#'   x1 = runif(n, min = -3, max = 3)
#'   x2 = x1^2 + rnorm(n, mean = 0, sd = 1)
#'   error <- rnorm(n, mean = 0, sd = 1)
#'   y <- x1 + x2 + error
#'   f <- ecdf(x1)
#'   x2 <- ifelse(runif(x2) < (f(x1) * 2 * pmissing), NA, x2)
#'   dat <- data.frame(y, x1, x2)
#'   SL.lib <- c("SL.glm", "SL.glm.interaction", "SL.glmnet", "SL.loess")
#'   imp.SL <- mice::mice(dat, m = 5, method = "SuperLearner",
#'                          print = TRUE, SL.library = SL.lib,
#'                          kernel = "gaussian",
#'                          bw = c(0.1, 0.2, 0.25, 0.3, 0.5, 1, 2.5, 5, 10, 20),
#'                          imputation = "semiparametricSL",
#'                          weights = "nadaraya-watson")
#'
#' @export
#' @import SuperLearner
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.SuperLearner = function(y, ry, x, wy = NULL, SL.library,
                                    kernel = c("gaussian", "uniform",
                                               "triangular"),
                                    bw = c(0.1, 0.2, 0.25, 0.3, 0.5, 1, 2.5,
                                           5, 10, 20),
                                    bw.update = TRUE,
                                    lambda = NULL,
                                    imputation = c("semiparametricSL",
                                                   "semiparametric",
                                                   "nonparametric"),
                                    weights = "nadaraya-watson", ...){
  if(!requireNamespace("SuperLearner")){
    stop(simpleError('SuperLearner is not installed.'))
  }

  # if(method.weights && is.null(.GlobalEnv$SuperMICE.weights)){
  #   .GlobalEnv$SuperMICE.weights <- list()
  # }

  if (is.null(wy)){
    wy <- !ry
  }

  if(length(unique(y)) == 2){
    imps = binary.SuperLearner(y, x, wy, SL.library, ...)
  }
  else if(class(y) == "numeric"){
    imps = continuous.SuperLearner(y, x, wy, SL.library, kernel = kernel,
                                   bw = bw, bw.update = bw.update,
                                   lambda = lambda, imputation = imputation,
                                   weights = weights, ...)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
