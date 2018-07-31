require(mice)
require(SuperLearner)


mice.impute.SuperLearner = function(y, ry, x, ...){
  args = list(...)
  # if(is.null(args$cvControl)){
  #   cvControl = list(V = 5L)
  # }
  # else{
  #   cvControl = args$cvControl
  #   args$cvControl = NULL
  # }
  # if(is.null(args$parallel)){
  #   parallel = "seq"
  # }
  # else{
  #   parallel = args$parallel
  #   args$parallel = NULL
  # }

  newdata <- data.frame(x[!ry,])
  names(newdata) = names(x)

  X <- data.frame(x[ry,])
  names(X) = names(x)
  Y <- y[ry]

  if(length(unique(y)) == 2){
    if(!is.null(args$h2o) && args$h2o){
      formals(h2o.init) <- args[names(args) %in% names(formals(h2o.init))]
      h2o.init()
      formals(h2o.stackedEnsemble) <- args[names(args) %in%
                                             names(formals(h2o.stackedEnsemble))]
      sl <- h2o.stackedEnsemble()
    }
    else{
      formals(SuperLearner) <- args[names(args) %in%
                                      names(formals(SuperLearner))]
      sl = SuperLearner(Y = Y, X = X, family = "binomial")
      p = predict(sl, newdata, Y, X, TRUE)$pred
      binaryImputations = rbinom(length(p), 1, p)
      unique(y)[binaryImputations + 1]
    }
  }
  else if(class(y) == "numeric"){
    if(!is.null(args$h2o) && args$h2o){
      namesArgs = names(args)
      namesh2o = names(formals(h2o.init))
      formals(h2o.init)[intersect(namesArgs, namesh2o)] <- args[
        intersect(namesArgs, namesh2o)]
      h2o.init()
      formals(h2o.stackedEnsemble) <- args[
        names(args) %in% names(formals(h2o.stackedEnsemble))]
      sl <- h2o.stackedEnsemble()
    }
    else{
      namesArgs = names(args)
      namesSL = names(formals(SuperLearner))
      formals(SuperLearner)[intersect(namesArgs, namesSL)] <- args[
        intersect(namesArgs, namesSL)]
      sl = SuperLearner(Y = Y, X = X, family = "gaussian")
      mu <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
      if(!is.null(args$CV) && args$CV){
        namesCVSL = names(formals(CV.SuperLearner))
        formals(CV.SuperLearner)[intersect(namesArgs, namesCVSL)] <-
          args[intersect(namesArgs, namesCVSL)]
        cv.sl = CV.SuperLearner(Y = Y, X = X, family = "gaussian")
        sd = sqrt(summary(cv.sl)$Table$Ave[1])
      }
      else{
        sd <- sqrt(mean((sl$SL.predict - y[ry])^2))
      }
      rnorm(length(mu), mu, sd)
    }
  }
  else{
    stop("Invalid data type for Super Learner Imputation.  Use only numeric
         or factors with two levels.")
  }
}

