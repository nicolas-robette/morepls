\name{plo_coef}
\alias{plo_coef}

\title{
Plot of coefficients
}

\description{
Plots the coefficients from a PLS regression.
}

\usage{
plo_coef(object, y = NULL, ncomp = NULL, sort = FALSE,
                     col = "darkgreen", repel = FALSE,
                     max.pval = NULL, whiskers = FALSE, ci = 0.95)
}

\arguments{
  \item{object}{an object of class \code{mvr} from \code{pls} package}
  \item{y}{the name of the response variable whose coefficients are plotted. If \code{NULL} (default), the first response variable is used.}
  \item{ncomp}{the number of components to use for computing coefficients}
  \item{sort}{logical. If \code{TRUE}, bars are sorted by decreasing coefficients. Default is \code{FALSE}.}
  \item{col}{color of the bars}
  \item{repel}{logical. If \code{TRUE}, the names of the variables are repelled with \code{geom_text_repel}. Default is \code{FALSE}}
  \item{max.pval}{coefficients with jack-knife p-values higher than \code{max.pval} have a more transparent color bar. If \code{NULL} (default), all bars have the same opacity. If not \code{NULL}, \code{object} must be cross-validated with \code{jackknife = TRUE}.}
  \item{whiskers}{logical. If \code{TRUE}, whiskers are added to represent the confidence interval of the coefficients. Default is \code{FALSE}. If \code{TRUE}, \code{object} must be cross-validated with \code{jackknife = TRUE}.}
  \item{ci}{the confidence level of the confidence interval. Only used if \code{whiskers} is \code{TRUE}. Default is 0.95.}
}

\value{
a \code{ggplot2} object
}

\references{
Martens, H., Næs, T. (1989) \emph{Multivariate calibration}. Chichester: Wiley.

Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et Pratique}. Editions TECHNIP, Paris.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{plo_ctr}}, \code{\link{plo_vip}}, \code{\link[pls]{jack.test}},
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
plo_coef(pls)
plo_coef(pls, max.pval = 0.05)
plo_coef(pls, whiskers = TRUE)
}

\keyword{ multivariate }
\keyword{ misc }
