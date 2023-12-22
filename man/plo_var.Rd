\name{plo_var}
\alias{plo_var}

\title{
Plot of loadings
}

\description{
Plots the loadings of the variables of a PLS regression.
}

\usage{
plo_var(object, comps = 1:2, which = "both", col = NULL,
 size = 3.88, Yline = TRUE, col.Yline = "firebrick3")
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{comps}{the components to use. Default is \code{c(1,2)}.}
  \item{which}{character string. If \code{"both"} (default), X and Y variables are plotted. If \code{"X"}, only X variables are plotted. If \code{"Y"}, only Y variables are plotted.}
  \item{col}{colors for the names of the variables. Only one value should be provided if \code{which} is "X" or "Y", a vector of two if \code{which} is "both". If \code{NULL} (default), colors are set automatically.}
  \item{size}{numerical value. The size of the names of the variables.}
  \item{Yline}{logical. If \code{TRUE} (default), a line is drawn through the origin and the coordinates of the response variable, and a second line orthogonal to the first one. This is aimed at facilitating the interpretation.}
  \item{col.Yline}{the color of the lines drawn if \code{Yline} is \code{TRUE}. Default is "firebrick3".}
}

\value{
a \code{ggplot2} object
}

\note{
This is what Tenenhaus calls the multivariate interpretation of the PLS components, as opposed to the univariate interpretation provided by the correlations (see \code{plo_cor}). This superposes Y loadings (vectors from the C matrix) and projections, i.e. modified weights (vectors of the W* matrix).
}

\seealso{
\code{\link{plo_cor}}
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
plo_var(pls)
}

\keyword{ multivariate }
\keyword{ misc }
