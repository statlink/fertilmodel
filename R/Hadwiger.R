Hadwiger <- function(rate, age) {

  fun <- function(para, age) {
    a <- para[1]  ;  b <- para[2]  ;  ca <- para[3]
    fx <- (a * b / ca) * ( ca / age )^1.5 *exp( -b^2 * (ca/age + age/ca - 2) )
    sum( (rate - fx)^2 )
  }

  ini <- c( sum(rate), max(rate), mean(age) )
  mod1 <- optim( ini, fun, age = age, control = list(maxit = 1000) )
  mod2 <- optim( mod1$par, fun, age = age, control = list(maxit = 1000) )
  while ( mod1$value - mod2$value > 1e-7 ) {
     mod1 <- mod2
     mod2 <- optim( mod1$par, fun, age = age, control = list(maxit = 1000) )
  }

  para <- mod2$par
  names(para) <- c("a", "b", "c")
  a <- para[1]  ;  b <- para[2]  ;  ca <- para[3]
  fit <- (a * b / ca) * ( ca / age )^1.5 *exp( -b^2 * (ca/age + age/ca - 2) )

  list(param = para, sse = mod2$value, fit = fit, res = rate - fit)

}
