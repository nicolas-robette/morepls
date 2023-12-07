\name{get_red}
\alias{get_red}

\title{
R2 and redundancies
}

\description{
Computes R2 and redundancies between variables and scores from a PLS regression.
}

\usage{
get_red(object)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package.}
}

\value{
A list with the following elements :
\item{Xt}{R2 and redundancies between X variables and X scores}
\item{Yt}{R2 and redundancies between Y variables and X scores}
\item{Xu}{R2 and redundancies between X variables and Y scores}
\item{Yu}{R2 and redundancies between Y variables and Y scores}
\item{Xtcum}{cumulative R2 and redundancies between X variables and X scores}
\item{Ytcum}{cumulative R2 and redundancies between Y variables and X scores}
\item{Xucum}{cumulative R2 and redundancies between X variables and Y scores}
\item{Yucum}{cumulative R2 and redundancies between Y variables and Y scores}
}

\references{
Martens, H., Næs, T. (1989) Multivariate calibration. Chichester: Wiley.

Tenenhaus, M. (1998) La Regression PLS. Theorie et Pratique. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{get_cor}}
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls")
res <- get_red(pls)
str(res)
}

\keyword{ multivariate }
\keyword{ misc }
