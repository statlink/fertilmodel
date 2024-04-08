Model2 <- function(rate, age) {

  ep <- which.max(rate)
  rate1 <- rate[1:c(ep - 2)]
  rate2 <- rate[-c( 1:c(ep - 2) )]

  fun <- function(para, age) {
    para[c(1, 2 ,5, 6)] <- exp(para[c(1, 2, 5, 6)])
    c1 <- para[1]   ;   c2 <- para[2]
    m1 <- para[3]   ;   m2 <- para[4]
    s11 <- para[5]  ;   s12 <- para[6]
    fx <- c1 * exp( - (age - m1)^2 / s11^2 ) + c2 * exp( - (age - m2)^2 / s12^2 )
    sum( (rate - fx)^2 )
  }

  ini <- c( 0, 0, mean( age[1:c(ep - 2)] ), mean( age[-c(1:c(ep - 2))] ), log( sd(rate1) ), log( sd(rate2) ) )
  mod1 <- optim(ini, fun, age = age, control = list(maxit = 2000) )
  mod2 <- optim(mod1$par, fun, age = age, control = list(maxit = 2000) )
  while ( mod1$value - mod2$value > 1e-7 ) {
    mod1 <- mod2
    mod2 <- optim( mod1$par, fun, age = age, control = list(maxit = 1000) )
  }

  para <- mod2$par
  para[c(1, 2, 5, 6)] <- exp(mod2$par[c(1, 2, 5, 6)])
  names(para) <- c("c1", "c2", "m1", "m2", "s11", "s12")
  c1 <- para[1]   ;   c2 <- para[2]
  m1 <- para[3]   ;   m2 <- para[4]
  s11 <- para[5]  ;   s12 <- para[6]
  fit <- c1 * exp( - (age - m1)^2 / s11^2 ) + c2 * exp( - (age - m2)^2 / s12^2 )

  list(param = para, sse = mod2$value, fit = fit, res = rate - fit)

}
