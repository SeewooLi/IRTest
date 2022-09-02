
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IRTest

<!-- badges: start -->

[![R-CMD-check](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/IRTest)](https://CRAN.R-project.org/package=IRTest)
<!-- badges: end -->

**IRTest** can be a useful tool for IRT (item response theory) parameter
estimation, especially when the violation of normality assumption on
latent distribution is suspicious.  
**IRTest** deals with uni-dimensional latent variable.  
In **IRTest**, along with the conventional approach that assumes
normality on latent distribution, several methods can be applied for
estimation of latent distribution: empirical histogram method,
two-component Gaussian mixture distribution, Davidian curve, kernel
density estimation.

## Installation

You can install IRTest on `R` console with:

``` r
install.packages("IRTest")
```

## Example

A simulation study for a Rasch model can be done in following manners:

``` r
library(IRTest)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
