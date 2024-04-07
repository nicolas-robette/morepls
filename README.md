# **morepls** <img src="man/figures/morepls.png" height=140px width=120px alt="" align="right" />

## Interpretation tools for PLS regression

<!-- badges: start -->
[![R-CMD-check](https://github.com/nicolas-robette/morepls/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nicolas-robette/morepls/actions/workflows/R-CMD-check.yaml)
  [![](https://www.r-pkg.org/badges/version/morepls?color=blue)](https://cran.r-project.org/package=morepls)
  [![](http://cranlogs.r-pkg.org/badges/last-month/morepls?color=orange)](https://cran.r-project.org/package=morepls)
<!-- badges: end -->

`morepls` provides functions for the interpretation of PLS regressions.

Graphical functions :

-   two-dimensional plot of observations
-   two-dimensional plot of correlations between variables and components
-   two-dimensional plot of variables (Y loadings and X projections)
-   bar plot of regression coefficients
-   bar plot of X variables weights
-   bar plot of X variables VIPs

Statistical indicators :

-   correlations between variables and components
-   R2 and redundancies between variables and components
-   Q2 and cumulative Q2 indexes
-   raw and standardized coefficients


## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/morepls")
```


## References

Martens, H., Næs, T. (1989) *Multivariate calibration*. Chichester: Wiley.

Tenenhaus, M. (1998) *La Regression PLS. Théorie et Pratique*. Editions TECHNIP, Paris.

The image in the hex sticker is outrageously taken from [https://moreplease.com/](https://moreplease.com/) from Iain Merrick.
