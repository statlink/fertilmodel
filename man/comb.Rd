\name{comb}
\alias{comb}

\title{
Model based average of the estimated values from two or more fertility models
}

\description{
Model based average of the estimated values from two or more fertility models.
}

\usage{
comb(models)
}

\arguments{
\item{models}{
A list with possible models.
}
}

\value{
A list including:
\item{weights}{
The weights assigned to each model.
}
\item{fit}{
The weighted fitted age-specific fertility rates \eqn{\hat{f}(x)}.
}
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{Hadwiger}
}
}

\examples{
rate <- c(0.0001, 0.0006, 0.0033, 0.0111, 0.0263, 0.0412, 0.0544, 0.0622,
0.0660, 0.0704,0.0723, 0.0753, 0.0814, 0.0873, 0.0924, 0.0962, 0.0989,
0.1006, 0.0990, 0.0933,0.0831, 0.0747, 0.0634, 0.0529, 0.0424, 0.0326,
0.0242, 0.0172, 0.0115, 0.0073, 0.0040, 0.0022, 0.0012, 0.0006, 0.0003,
0.0002, 0.0001)
age <- 13:49
mod1 <- Hadwiger(rate, age)
mod2 <- Gama(rate, age)
mod3 <- Model1(rate, age)
mod4 <- Model2(rate, age)
a <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
comb(a)
}


