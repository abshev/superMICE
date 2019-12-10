#Binary SuperLearner PMM
binary.SuperLearner.PMM = function(Y, X, newdata, SL.library, k, ...){
  args = c(list(Y = Y, X = X, family = binomial(), SL.library = SL.library),
           list(...))
  args$type = NULL
  sl <- do.call(SuperLearner, args)
  # sl <- SuperLearner(Y = Y, X = X, family = binomial(),
  #                    SL.library = SL.library, ...)
  p <- predict(object = sl, newdata = newdata, X = X, Y = Y, TRUE)$pred
  candidates <- FNN::get.knn(matrix(p, ncol = 1), k = k)
  randomCandidates <- apply(candidates$nn.index, 1,
                            function(x){x[sample(1:k, 1)]})
  Y[randomCandidates]
}
