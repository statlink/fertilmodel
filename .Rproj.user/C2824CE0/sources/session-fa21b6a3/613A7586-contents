comb <- function(models) {
  M <- length(models)
  r <- models[[ 1 ]]$res
  for (i in 2:M)  r <- cbind(r, models[[ i ]]$res)
  Amat <- t( rbind( matrix(1, nrow = 1, ncol = M), diag(M), -diag(M) ) )
  bvec <- rbind(1, matrix(0, nrow = M, ncol = 1), matrix(-1, nrow = M, ncol = 1) )
  Dmat <- crossprod(r)
  dmat <- matrix(0, M, 1)
  QP <- quadprog::solve.QP(Dmat, dmat, Amat, bvec, 1)
  weights <- QP$solution
  weights <- weights * (weights > .001)
  f <- models[[1]]$fit
  for (i in 2:M)  f <- cbind(f, models[[ i ]]$fit)
  fit <- f %*% weights
  list(weights = weights, fit = fit)
}
