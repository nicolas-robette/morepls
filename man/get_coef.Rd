\name{get_coef}
\alias{get_coef}

\title{
Standardized and raw coefficients
}

\description{
Computes the standardized and raw coefficients of a PLS regression, with p-values and confidence intervals from a jackknife procedure.
}

\usage{
get_coef(object, y = NULL, ncomp = NULL,
 ci = 0.95, raw = FALSE)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package. It must be cross-validated with \code{jackknife = TRUE}}
  \item{y}{the name of the response variable whose coefficients are plotted. If \code{NULL} (default), the first response variable is used.}
  \item{ncomp}{the number of components to use for computing coefficients}
  \item{ci}{the confidence level of the confidence interval. Default is 0.95.}
  \item{raw}{logical. If \code{FALSE} (default), standardized coefficients are computed. If \code{TRUE}, raw coefficients are computed.}
}

\value{
A data frame with coefficients, standard deviation, t-values, p-values and confidence intervals.
}

\references{
Martens, H., Næs, T. (1989) Multivariate calibration. Chichester: Wiley.

Tenenhaus, M. (1998) La Regression PLS. Theorie et Pratique. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{plo_coef}}
}

\examples{
library(pls)
data(yarn)
pls <- mvr(density ~ NIR,
           ncomp = 5,
           data = yarn,
           validation = "CV",
           method = "oscorespls",
           jackknife = TRUE)
res <- get_coef(pls)
head(res)
}

\keyword{ multivariate }
\keyword{ misc }
