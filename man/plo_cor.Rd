\name{plo_cor}
\alias{plo_cor}

\title{
Plot of correlations
}

\description{
Plots the correlations between (X and Y) variables and the components (X scores) of a PLS regression.
}

\usage{
plo_cor(object, comps = 1:2, which = "both", min.cor = NULL,
        lim = NULL, circles = NULL, col = NULL, size = 3.88)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{comps}{the components to use. Default is \code{c(1,2)}.}
  \item{which}{character string. If \code{"both"} (default), X and Y variables are plotted. If \code{"X"}, only X variables are plotted. If \code{"Y"}, only Y variables are plotted.}
  \item{min.cor}{numerical value. The minimal correlation with one or the other component for a variable to be plotted. If \code{NULL} (default), all the variables are plotted.}
  \item{lim}{numerical value. The limit of the scale (in absolute value). If \code{NULL} (default), the limits are automatically determined from the range of tha data.}
  \item{circles}{vector of numeric values. Circles are added to the plot at radiuses specified in \code{circles}. If \code{NULL} (default), no circle is plotted.}
  \item{col}{colors for the names of the variables. Only one value should be provided if \code{which} is "X" or "Y", a vector of two if \code{which} is "both". If \code{NULL} (default), colors are set automatically.}
  \item{size}{numerical value. The size of the names of the variables.}
}

\value{
a \code{ggplot2} object
}

\note{
This is what Tenenhaus calls the univariate interpretation of the PLS components, as opposed to the multivariate interpretation (see \code{plo_var}).
}

\seealso{
\code{\link{get_cor}}, \code{\link{plo_var}}
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
plo_cor(pls)
# plot with circles corresponding to
# correlations of 0.5 and 1
plo_cor(pls, lim = 1, circles = c(0.5, 1), col = c("pink", "purple"))
}

\keyword{ multivariate }
\keyword{ misc }
