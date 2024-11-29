Gama <- function(rate, age) {

  fun <- function(para, age) {
    R <- para[1]   ;  b <- para[2]
    ca <- para[3]  ;  d <- para[4]
    fx <- R * ( 1 / ( gamma(b) * ca^b ) ) * (age - d)^(b - 1) * exp( -(age - d) / ca )
    sum( (rate - fx)^2 )
  }

  ini <- c( mean(rate), 1, var(rate), min(age) )
  mod1 <- optim(ini, fun, age = age, control = list(maxit = 2000) )
  mod2 <- optim(mod1$par, fun, age = age, control = list(maxit = 2000) )
  while ( mod2$value - mod1$value > 1e-7 ) {
    mod1 <- mod2
    mod2 <- optim( mod1$par, fun, age = age, control = list(maxit = 1000) )
  }

  para <- mod2$par
  names(para) <- c("R", "b", "c", "d")
  R <- para[1]   ;  b <- para[2]
  ca <- para[3]  ;  d <- para[4]
  fit <- R * ( 1 / ( gamma(b) * ca^b ) ) * (age - d)^(b - 1) * exp( -(age - d) / ca )

  list(param = para, sse = mod2$value, fit = fit, res = rate - fit)

}
