fertil.plot <- function(rate, age, fit = NULL, grid = FALSE, names = NULL) {

  if ( is.null(fit) ) {
    if (grid) {
      plot(age, rate, ylim = c( min(rate), max(rate) ), xlab = "Age of the mother", ylab = "Fertility rate",
           pch = 16, cex.lab = 1.3, xaxt = "n", yaxt = "n")
      axis(1, at = age, labels = age)
      ra <- seq(min(rate), max(rate), length = 10)
      axis(2, at = ra, labels = round(ra, 3) )
      abline(v = age, col = "lightgrey", lty = 2)
      abline(h = ra, col = "lightgrey", lty = 2)
      points(age, rate, pch = 16)
      lines(age, rate, lwd = 2)
    } else {
      plot(age, rate, xlab = "Age of the mother", ylab = "Fertility rate", pch = 16, cex.lab = 1.3)
      lines(age, rate, lwd = 2)
    }

  } else {
    cb <- cbind(rate, fit)

    if (grid) {
      plot(age, rate, xlab = "Age of the mother", ylab = "Fertility rate", pch = 16, cex.lab = 1.3, xaxt = "n", yaxt = "n")
      axis(1, at = age, labels = age)
      ra <- seq(min(cb), max(cb), length = 10)
      axis(2, at = ra, labels = round(ra, 3) )
      abline(v = age, col = "lightgrey", lty = 2)
      abline(h = ra, col = "lightgrey", lty = 2)
      points(age, rate, pch = 16)
    } else {
      plot(age, rate, xlab = "Age of the mother", ylab = "Fertility rate", pch = 16, cex.lab = 1.3)
    }
    lines(age, rate, lwd = 2)
    for ( i in 2:ncol(cb) ) {
      points(age, cb[, i], col = i, pch = 16)
      lines(age, cb[, i], col = i, lwd = 2)
    }
    if ( !is.null(names) ) {
      names <- c("Observed", names)
      legend("topright", names, col = 1:ncol(cb), lwd = rep(2, ncol(cb)), text.col = 1:ncol(cb) )
    }
  }

}
