\name{get_cor}
\alias{get_cor}

\title{
Correlations between variables and scores
}

\description{
Computes correlations between variables and scores from a PLS regression.
}

\usage{
get_cor(object)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package.}
}

\value{
A list with the following elements :
\item{Xt}{correlations between X variables and X scores}
\item{Yt}{correlations between Y variables and X scores}
\item{Xu}{correlations between X variables and Y scores}
\item{Yu}{correlations between Y variables and Y scores}
\item{XY}{correlations between X variables and Y variables}
\item{tu}{correlations between X scores and Y scores}
}

\references{
Martens, H., Næs, T. (1989) Multivariate calibration. Chichester: Wiley.

Tenenhaus, M. (1998) La Regression PLS. Theorie et Pratique. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{get_red}}, \code{\link{plo_cor}}
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls")
res <- get_cor(pls)
str(res)
}

\keyword{ multivariate }
\keyword{ misc }
