# **morepls**

## Interpretation tools for PLS regression

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
