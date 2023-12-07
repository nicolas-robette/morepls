\name{get_Q2}
\alias{get_Q2}

\title{
Q2 and cumulative Q2 indexes
}

\description{
Computes Q2 and cumulative Q2 indexes from a PLS regression.
}

\usage{
get_Q2(object)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package. It has to be cross-validated}
}

\value{
A list with the following elements :
\item{Q2kh}{Q2 index by X variable and number of components}
\item{Q2h}{Q2 index by number of components}
\item{Q2cumkh}{cumulative Q2 index by X variable and number of components}
\item{Q2cumh}{cumulative Q2 index by number of components}
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
res <- get_Q2(pls)
str(res)
}

\keyword{ multivariate }
\keyword{ misc }
