\name{plo_obs}
\alias{plo_obs}

\title{
Plot of scores
}

\description{
Plots the scores of the observations of a PLS regression.
}

\usage{
plo_obs(object, comps = 1:2, col = "black", size = 1.5)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{comps}{the components to use. Default is \code{c(1,2)}.}
  \item{col}{the color of the points.}
  \item{size}{numerical value. The size of the points.}
}

\value{
a \code{ggplot2} object
}

\references{
Martens, H., Næs, T. (1989) Multivariate calibration. Chichester: Wiley.

Tenenhaus, M. (1998) La Regression PLS. Theorie et Pratique. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls")
plo_obs(pls)
}

\keyword{ multivariate }
\keyword{ misc }
