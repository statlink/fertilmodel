\name{Hadwiger}
\alias{Hadwiger}
\alias{Gama}
\alias{Model1}
\alias{Model2}

\title{
Fertility models
}

\description{
Fertility models.
}

\usage{
Hadwiger(rate, age)
Gama(rate, age)
Model1(rate, age)
Model2(rate, age)
}

\arguments{
\item{rate}{
A vector with the age-specific fertility rates.
}
\item{age}{
A vector with the age of the women.
}
}

\details{
The following fertility models are fitted:
Hadwiger:
\deqn{
f(x)=\frac{ab}{c}(\frac{c}{x})^{3/2}\exp[-b^2(\frac{c}{x}+\frac{x}{c}-2)],
}
where \eqn{x} is the age of the mother at birth, \eqn{a} is associated with total fertility, the parameter \eqn{b} determines the height of the curve and the parameter \eqn{c} is related to the mean age of motherhood.

Gama:
\deqn{
f(x)=R\frac{1}{\Gamma(b)c^b}(x-d)^{b-1}\exp(-\frac{x-d}{c}),
}
where \eqn{d} represents the lower age at childbearing, while the parameter \eqn{R} determines the
level of fertility.

Model1:
\deqn{
f(x)=c_1\exp[-\frac{(x-\mu)^2}{\sigma^2(x)}],
}
where \eqn{\sigma(x)=\sigma_{11}} if \eqn{x \leq \mu} and \eqn{\sigma(x)=\sigma_{12}} if \eqn{x>\mu}. The parameter \eqn{c_1} describes the base level of the fertility curve and is associated with the total fertility rate, \eqn{\mu} reflects the location of the distribution, i.e. the modal age and \eqn{\sigma_{11}} and \eqn{\sigma_{12}} reflect the spread of the distribution before and after its peak, respectively.

Model2:
\deqn{
f(x)=c_1\exp[-\frac{(x-\mu_1)^2}{\sigma_1^2}] + c_2\exp[-\frac{(x-\mu_2)^2}{\sigma_2^2}],
}
where the parameters \eqn{c_1} and \eqn{c_2} express the severity i.e. the total fertility rates of the first
and the second hump respectively, \eqn{\mu_1} and \eqn{\mu_2} are related to the mean ages of the two
subpopulations the one with earlier fertility and the other with fertility at later ages, while \eqn{\sigma_1} and \eqn{\sigma_2} reflect the variances of the two humps.
}

\value{
A list including:
\item{param}{
The vector of the estimated parameters.
}
\item{sse}{
The sum of squars of the errors \eqn{\sum_{i=1}^n(f_x-\hat{f}(x))^2}, where \eqn{f_x} denotes the observed age-specific fertility rates and \eqn{\hat{f}(x)} denote the fitted age-specific fertility rates.
}
\item{fx}{
The fitted values, the fitted age-specific fertility rates \eqn{\hat{f}(x)}.
}
\item{res}{
The residuals, \eqn{f_x-\hat{f}_x}.
}
}

\references{
Peristera P. and Kostaki A. (2007). Modeling fertility in modern populations. Demographic Research, 16(6): 141--194.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{fertil.plot}, \link{comb}
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
}


