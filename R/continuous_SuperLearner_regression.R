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
#' @param bw.update eriogjaoier
#' @param lambda NULL or numeric value for bandwidth for kernel (as half-width of the kernel).
#' @param kernel One of "gaussian",...  Kernel function used to compute weights.
#' @param ... further arguments passed to SuperLearner.
#' @return Numeric vector of randomly drawn imputed values.
#'


#Continuous SuperLearner Regression
continuousSuperLearner <- function(y, x, wy, SL.library, kernel, bw, bw.update,
                                    lambda, ...){
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

  if(length(bw) == 1 & class(bw) == "numeric"){
    bw <- as.list(rep(bw, times = sum(wy)))
  }
  else if(!bw.update){
    if(class(bw) == "numeric"){
        bw <- sapply((1:length(y))[wy], jackknifeBandwidthSelection,
                     bwGrid = bw,
                     preds = sl.preds,
                     y = y,
                     delta = as.numeric(!wy),
                     lambda = lambda,
                     kernel = kernel)
        bw <- as.list(bw)
    }
    p = parent.frame(2)
    p$args$bw <- bw
  }
  else{
    bw <- sapply((1:length(y))[wy], jackknifeBandwidthSelection,
                 bwGrid = bw,
                 preds = sl.preds,
                 y = y,
                 delta = as.numeric(!wy),
                 lambda = lambda,
                 kernel = kernel)
    bw <- as.list(bw)
  }


  sapply(1:sum(wy), localImputation, preds = sl.preds, y = y,
         delta = as.numeric(!wy),
         bw = bw, lambda = lambda,
         kernel = kernel)
}
