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
#'   imp.SL <- mice::mice(data, m = 5, method = "SuperLearner", print = TRUE,
#'                  SL.library = SL.lib, SL.CV = TRUE,
#'                  imputation.method = "regression",
#'                  SL.backend = "SuperLearner")
#' @export
#' @import SuperLearner
#' @import h2o
#' @importFrom stats gaussian
#' @importFrom stats binomial

mice.impute.SuperLearner = function(y, ry, x, wy = NULL, SL.library,
                                    SL.CV = TRUE,
                                    imputation.method = c("regression", "PMM"),
                                    SL.backend = c("SuperLearner", "h2o"),
                                    donors = 5L, ...){
  SuperLearnerPackage <- match.arg(SL.backend)
  method <- match.arg(imputation.method)

  if(SuperLearnerPackage == "SuperLearner"){
    if(!requireNamespace("SuperLearner")){
      stop(simpleError('SuperLearner is not installed.'))
    }
  } else if(SuperLearnerPackage == "h2o"){
    if(!requireNamespace("h2o")){
      stop(simpleError('h2o is not installed.'))
    }
  } else{
    stop(simpleError('Super Learner backend not supported.'))}

  if (is.null(wy)){
    wy <- !ry
  }

  newdata <- data.frame(x[wy,])
  names(newdata) = sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) = sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

  if(length(unique(y)) == 2){
    if(method == "regression"){
      if(SLbackend == "h2o"){


        formals(h2o.stackedEnsemble) <- args[names(args) %in%
                                               names(formals(h2o.stackedEnsemble))]
        sl <- h2o.stackedEnsemble()
      }
      else if(SuperLearnerPackage == "SuperLearner"){

      }
    }
    else if(method == "PMM"){
      if(SuperLearnerPackage == "h2o"){

      }
      else if(SuperLearnerPackage == "SuperLearner"){

      }
    }
  }
  else if(class(y) == "numeric"){
    if(method == "regression"){
      if(SuperLearnerPackage == "h2o"){
        imps = continuous.H2O.regression(Y, X, newdata, SL.library, ...)
      }
      else if(SuperLearnerPackage == "SuperLearner"){
        imps = continuous.SuperLearner.regression(Y, X, newdata, SL.library,
                                                  SL.CV, ...)
      }
    }
    else if(method == "PMM"){
      if(SuperLearnerPackage == "h2o"){
        imps = continuous.H2O.PMM(Y, X, newdata, SL.library, k = donors, ...)
      }
      else if(SuperLearnerPackage == "SuperLearner"){
        imps = continuous.SuperLearner.PMM(Y, X, newdata, SL.library,
                                           k = donors, ...)
      }


    }

  }
  else{
    stop(simpleError("Invalid data type for Super Learner Imputation.
          Use only numeric or factors with two levels (binary) data types."))
  }
  return(imps)
}

# #Continuous SuperLearner Regression
# continuous.SuperLearner.regression = function(Y, X, newdata, SL.library, SL.CV,
#                                               ...){
#   args = c(list(Y = Y, X = X, family = gaussian(), SL.library = SL.library),
#            list(...))
#   args$type = NULL
#   # namesArgs = names(args)
#   # namesSL = names(formals(SuperLearner))
#   # formalsSL = formals(SuperLearner)
#   # formalsSL[intersect(namesArgs, namesSL)] <- args[intersect(namesArgs, namesSL)]
#   sl <- do.call(SuperLearner, args)
#   # sl <- SuperLearner(Y = Y, X = X, family = gaussian(),
#   #                   SL.library = SL.library, ...)
#   mu <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
#   if(SL.CV){
#     # namesCVSL = names(formals(CV.SuperLearner))
#     # formals(CV.SuperLearner)[intersect(namesArgs, namesCVSL)] <-
#       # args[intersect(namesArgs, namesCVSL)]
#     # cv.sl = CV.SuperLearner(Y = Y, X = X, family = gaussian(),
#                             # SL.library = SL.library)
#     cv.sl = do.call(CV.SuperLearner, args)
#     MSE <- summary(cv.sl)$Table$Ave[1]
#     sd <- sqrt(MSE * (1 + 1 / nrow(X)))
#   }
#   else{
#     MSE <- mean((sl$SL.predict - y[!wy])^2)
#     sd <- sqrt(MSE * (1 + 1 / nrow(X)))
#   }
#   rnorm(length(mu), mu, sd)
# }

#Continuous SuperLearner PMM
# continuous.SuperLearner.PMM = function(Y, X, newdata, SL.library, k, ...){
#   args = c(list(Y = Y, X = X, family = gaussian(), SL.library = SL.library),
#            list(...))
#   args$type = NULL
#   sl <- do.call(SuperLearner, args)
#   # sl <- SuperLearner(Y = Y, X = X, family = gaussian(),
#   #                   SL.library = SL.library, ...)
#   mu <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
#   candidates <- FNN::get.knn(matrix(mu, ncol = 1), k = k)
#   randomCandidates <- apply(candidates$nn.index, 1,
#                            function(x, k){x[sample(1:k, 1)]}, k = k)
#   Y[randomCandidates]
# }

# #Binary SuperLearner regression
# binary.SuperLearner.regression = function(Y, X, newdata, SL.library, SL.CV,
#                                           ...){
#   args = c(list(Y = Y, X = X, family = binomial(), SL.library = SL.library),
#            list(...))
#   args$type = NULL
#   sl <- do.call(SuperLearner, args)
#   # sl <- SuperLearner(Y = Y, X = X, family = binomial(),
#   #                   SL.library = SL.library, ...)
#   p <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
#   binaryImputations = rbinom(length(p), 1, p)
#   if(is.factor(Y)){
#     levels(Y)[binaryImputations + 1]
#   }
#   else if(is.logical(Y)){
#     c(FALSE, TRUE)[binaryImputations + 1]
#   }
#   else if(is.numeric(Y) | is.character(Y)){
#     sort(Y)[binaryImputations + 1]
#   }
#   else{
#     unique(Y)[binaryImputations + 1]
#   }
# }

# #Binary SuperLearner PMM
# binary.SuperLearner.PMM = function(Y, X, newdata, SL.library, k, ...){
#   args = c(list(Y = Y, X = X, family = binomial(), SL.library = SL.library),
#            list(...))
#   args$type = NULL
#   sl <- do.call(SuperLearner, args)
#   # sl <- SuperLearner(Y = Y, X = X, family = binomial(),
#   #                    SL.library = SL.library, ...)
#   p <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
#   candidates <- FNN::get.knn(matrix(p, ncol = 1), k = k)
#   randomCandidates <- apply(candidates$nn.index, 1,
#                             function(x){x[sample(1:k, 1)]})
#   Y[randomCandidates]
# }

#Continuous H2O regression
# continuous.H2O.regression = function(Y, X, newdata, SL.library){
#   h2oModels = lapply(SL.library, function(h2oArgs, EX, Y){
#     f.h2o = h2oArgs[[1]]
#     if(is.null(h2oArgs$keep_cross_validation_predictions) ||
#        !h2oArgs$keep_cross_validation_predictions){
#       h2oArgs$keep_cross_validation_predictions = TRUE
#     }
#     if(is.null(h2oArgs$nfolds)){
#       h2oArgs$nfolds = 5
#     }
#     if(!is.null(h2oArgs$interaction_pairs)){
#       h2oArgs$interaction_pairs = lapply(h2oArgs$interaction_pairs,
#                                          function(pair, varNames){
#                                            c(varNames[pair[1]],
#                                              varNames[pair[2]])
#                                          }, varNames = colnames(EX))
#     }
#     f.args = c(list(x = 2:(ncol(EX) + 1), y = 1,
#                     training_frame = as.h2o(data.frame(cbind(Y, EX)))),
#                h2oArgs[-1])
#     do.call(f.h2o, f.args)
#   }, EX = X, Y = Y)
#
#   namesArgs = names(args)
#   namesSL = names(formals(h2o.stackedEnsemble))
#   formals(h2o.stackedEnsemble)[intersect(namesArgs, namesSL)] <- args[
#     intersect(namesArgs, namesSL)]
#   sl <- h2o.stackedEnsemble(x = 2:(ncol(X) + 1), y = 1,
#                             training_frame = as.h2o(data.frame(cbind(Y, X))),
#                             base_models = h2oModels)
#   # if(is.null(h2oArgs$interaction_pairs)){
#   mu <- as.vector(h2o.predict(sl, newdata = as.h2o(newdata)))
#   # }
#   # else{
#   #   newdata
#   #   mu <- as.vector(h2o.predict(sl, newdata = as.h2o(newdata)))
#   # }
#
#   MSE <- sl@model$training_metrics@metrics$MSE
#   sd <- sqrt(MSE * (1 + 1 / nrow(X)))
#
# }

#Continuous H2O PMM
# continuous.H2O.PMM = function(Y, X, newdata, SL.library, k){
#
# }

#Binary H2O Regression
# binary.H2O.regression = function(){
#
# }

#Binary H2O PMM
# binary.H2O.PMM = function(){
#
# }


# h2oDefaults = function()

