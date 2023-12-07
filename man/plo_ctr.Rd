\name{plo_ctr}
\alias{plo_ctr}

\title{
Plot of weights
}

\description{
Plots the weights of X variables from a PLS regression.
}

\usage{
plo_ctr(object, comp = 1, sort = FALSE, col = "tomato4", repel = FALSE)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{comp}{the component to use. Default is 1.}
  \item{sort}{logical. If \code{TRUE}, bars are sorted by decreasing VIPs. Default is \code{FALSE}.}
  \item{col}{color of the bars}
  \item{repel}{logical. If \code{TRUE}, the names of the variables are repelled with \code{geom_text_repel}. Default is \code{FALSE}}
}

\value{
a \code{ggplot2} object
}

\details{
According to Tenenhaus, the contribution of a variable to the construction of a component is measured by the squared loading weight. For a given component, the sum af the squared loading weights is equal to 1. This plot represents the loading weights, which keeps the information about their sign. Dashed lines are added at +/- sqrt(1/p), with p the number of X variables, which corresponds to the average contribution to the construction of the component.
}

\references{
Martens, H., Næs, T. (1989) Multivariate calibration. Chichester: Wiley.

Tenenhaus, M. (1998) La Regression PLS. Theorie et Pratique. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{plo_coef}}, \code{\link{plo_vip}}
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls")
plo_ctr(pls)
}

\keyword{ multivariate }
\keyword{ misc }
