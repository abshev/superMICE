#' SuperLearner method for \code{mice} package.
#'
#' Method for the \code{mice} package that uses SuperLearner as the predctive
#' algorithm.  Model fitting is done using the \code{SuperLearner} package.
#'
#' @param y Vector to be imputed
#' @param ry Logical vector of length `length(y)` indicating the the subset
#' \code{y[ry]} of elements in y to which the imputation model is fitted.
#' The \code{ry} generally distinguishes the observed (\code{TRUE}) and missing
#' values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors
#' for \code{y}.
#' Matrix \code{x} may have no missing values.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value indicates
#' locations in \code{y} for which imputations are created.
#' @param SL.library For SuperLearner: Either a character vector of prediction
#' algorithms or list containing character vectors as specified by the
#' SuperLearner package.  See details below.
#' @param kernel One of "gaussian", "uniform", "triangular".  Kernel function
#' used to compute weights.
#' @param bw NULL or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param bw.update logical indicating whether bandwidths should be computed
#' every iteration or only on the first iteration.  Default is \code{TRUE},
#' but \code{FALSE} may speed up the run time at the cost of accuracy.
#' @param ... Further arguments passed to \code{SuperLearner}.
#' @return Vector with imputed data, same type as \code{y}, and of length \code{sum(wy)}
#'
#' @details SuperMICE is a method for use with the [mice()][mice::mice()] function that
#' implements the ensemble predictive model, SuperLearner (van der Laan, 2011),
#' into the mice (van Buuren, 2011) multiple imputation procedure. An estimate
#' for missing values are obtained by fitting  a selection of different
#' predictive models on complete cases and determining an optimal weighted average
#' of candidate models to predict the missing cases. SuperMICE uses the implementation
#' of SuperLearner found in the [SuperLearner] package.
#' The models to be used with [`SuperLearner()`][SuperLearner::SuperLearner()] are supplied by the user as a
#' character vector. For a full list of available methods see
#' [`listWrappers()`][SuperLearner::listWrappers()].
#'
#' By design, the SuperLearner method only provides a point estimate for predictive purposes.
#' To generate an appropriate neighborhood around the SuperLearner estimate to draw
#' from we use a kernel based estimate of local variance around each missing value.
#' The kernel can be set by the user with the \code{kerel} argument as either
#' a gaussian kernel, unfiform kernel, or triangular kernel.  The user must also
#' supply a list of candidate bandwidths in the \code{bw} argument as a numeric
#' vector.  For more information on the variance and bandwidth selection
#' see Laqueur, et. al (2021). In every iteration the mice procedure, the optimal
#' bandwidth is reselected. This may be changed to select the bandwidth only
#' on the first iteration to speed up the total run time of the imputation by
#' changing \code{bw.update} to \code{FALSE}; however this may bias your results.
#' Note that this only applies to continuous response variables.  In the binary
#' case the variance is a function of the SuperLearner estimate.
#'
#'
#' @references
#' Laqueur, H. S., Shev, A. B., Kagawa, R. M. C. (2021). SuperMICE: An Ensemble
#' Machine Learning Approach to Multiple Imputation by Chained Equations.
#' American Journal of Epidemiology, kwab271,
#' doi: [10.1093/aje/kwab271](https://doi.org/10.1093/aje/kwab271).
#'
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. Journal of Statistical Software,
#' **45**(3), 1-67. doi: [10.18637/jss.v045.i03](https://doi.org/10.18637/jss.v045.i03).
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner,
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#'
#' @seealso [`mice()`][mice::mice()], [`SuperLearner()`][SuperLearner::SuperLearner()]
#'
#' @examples
#'   library(mice)
#'   library(superMICE)
#'
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
#'   imp.SL <- mice(dat, m = 5, method = "SuperLearner",
#'                          print = TRUE, SL.library = SL.lib,
#'                          kernel = "gaussian",
#'                          bw = c(0.1, 0.2, 0.25, 0.3, 0.5, 1, 2.5, 5, 10, 20))
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
                                    bw.update = TRUE, ...){
  if(!requireNamespace("SuperLearner")){
    stop(simpleError('SuperLearner is not installed.'))
  }

  if (is.null(wy)){
    wy <- !ry
  }

  if(length(unique(y)) == 2){
    imps = binarySuperLearner(y, x, wy, SL.library, ...)
  }
  else if(class(y) == "numeric"){
    imps = continuousSuperLearner(y, x, wy, SL.library, kernel = kernel,
                                   bw = bw, bw.update = bw.update, ...)
  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}
