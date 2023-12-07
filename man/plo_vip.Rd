\name{plo_vip}
\alias{plo_vip}

\title{
Plot of VIPs
}

\description{
Plots the Variable Importance in Projections (VIP) indexes of a PLS regression.
}

\usage{
plo_vip(object, ncomp = NULL, sort = FALSE,
col = "steelblue4", repel = FALSE)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{ncomp}{the number of components to use for computing VIPs}
  \item{sort}{logical. If \code{TRUE}, bars are sorted by decreasing VIPs. Default is \code{FALSE}.}
  \item{col}{color of the bars}
  \item{repel}{logical. If \code{TRUE}, the names of the variables are repelled with \code{geom_text_repel}. Default is \code{FALSE}}
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

\seealso{
\code{\link[plsVarSel]{VIP}}
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls")
plo_vip(pls)
}

\keyword{ multivariate }
\keyword{ misc }
