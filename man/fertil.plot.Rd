\name{fertil.plot}
\alias{fertil.plot}

\title{
Plot of the age-specific fertility rates and the estimated values from one or more fertility models
}

\description{
Plot of the age-specific fertility rates and the estimated values from one or more fertility models.
}

\usage{
fertil.plot(rate, age, fit = NULL, grid = FALSE, names = NULL)
}

\arguments{
\item{rate}{
A vector with the age-specific fertility rates.
}
\item{age}{
A vector with the age of the women.
}
\item{fit}{
Here you can specify nothing (omly plot the fertility rates across the ages), or you can specify a vector or a matrix with fitted values from at least one model.
}
\item{grid}{
Do you want a grid of vertical and horizontal lines? TRUE or FALSE.
}
\item{names}{
If you provided fitted models from a model, you can specify the name(s) of the model(s) so that they appear as a legend.
}
}


\value{
A plot with the age-specific fertility rates across the mothers' age and perhaps the fitted values from at least one model.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{Hadwiger}, \link{comb}
}
}

\examples{
rate <- c(0.0000, 0.0005, 0.0023, 0.0073, 0.0175, 0.0283,
0.0420, 0.0523, 0.0601, 0.0712, 0.0789, 0.0865, 0.0939,
0.0991, 0.1052, 0.1115, 0.1155, 0.1178, 0.1211, 0.1159,
0.1104, 0.1031, 0.0916, 0.0776, 0.0639, 0.0498, 0.0387,
0.0281, 0.0185, 0.0117, 0.0067, 0.0038, 0.0020, 0.0011,
0.0007, 0.0003, 0.0002)
age <- 13:49
mod1 <- Hadwiger(rate, age)
mod2 <- Gama(rate, age)
fertil.plot(rate, age)
fertil.plot(rate, age, cbind(mod1$fit, mod2$fit), grid = TRUE, names = c("Hadwiger", "Gama") )

}


