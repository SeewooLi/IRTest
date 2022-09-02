
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

You can install IRTest on R-console with:

``` r
install.packages("IRTest")
```

## Example

A simulation study for a Rasch model can be done in following manners:

``` r
library(IRTest)
```

-   An artificial data of 500 examinees and 10 items.

``` r
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(1, 10),
                          N=500,
                          nitem_D = 10,
                          nitem_P = 0,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
initialitem <- Alldata$initialitem_D
theta <- Alldata$theta
```

-   Analysis

For an illustrative purpose, empirical histogram method is used for
estimation of latent distribution.

``` r
Mod1 <- IRTest_Dich(initialitem = initialitem,
                    data = data,
                    model = rep(1, 10),
                    latent_dist = "EHM",
                    max_iter = 200,
                    threshold = .0001)
#> Method = EHM, EM cycle = 1, Max-Change = 1.627312Method = EHM, EM cycle = 2, Max-Change = 0.1191157Method = EHM, EM cycle = 3, Max-Change = 0.06904811Method = EHM, EM cycle = 4, Max-Change = 0.04596045Method = EHM, EM cycle = 5, Max-Change = 0.03047993Method = EHM, EM cycle = 6, Max-Change = 0.02046156Method = EHM, EM cycle = 7, Max-Change = 0.01384977Method = EHM, EM cycle = 8, Max-Change = 0.009450715Method = EHM, EM cycle = 9, Max-Change = 0.006408359Method = EHM, EM cycle = 10, Max-Change = 0.004352839Method = EHM, EM cycle = 11, Max-Change = 0.002995963Method = EHM, EM cycle = 12, Max-Change = 0.002078356Method = EHM, EM cycle = 13, Max-Change = 0.001450056Method = EHM, EM cycle = 14, Max-Change = 0.001018085Method = EHM, EM cycle = 15, Max-Change = 0.0007197317Method = EHM, EM cycle = 16, Max-Change = 0.000512675Method = EHM, EM cycle = 17, Max-Change = 0.0003683793Method = EHM, EM cycle = 18, Max-Change = 0.0002675048Method = EHM, EM cycle = 19, Max-Change = 0.00019679Method = EHM, EM cycle = 20, Max-Change = 0.0001957776Method = EHM, EM cycle = 21, Max-Change = 0.0002028476Method = EHM, EM cycle = 22, Max-Change = 0.0002022226Method = EHM, EM cycle = 23, Max-Change = 0.0001967191Method = EHM, EM cycle = 24, Max-Change = 0.0001882029Method = EHM, EM cycle = 25, Max-Change = 0.0001779651Method = EHM, EM cycle = 26, Max-Change = 0.000166881Method = EHM, EM cycle = 27, Max-Change = 0.0001555367Method = EHM, EM cycle = 28, Max-Change = 0.0001443165Method = EHM, EM cycle = 29, Max-Change = 0.0001334642Method = EHM, EM cycle = 30, Max-Change = 0.0001231267Method = EHM, EM cycle = 31, Max-Change = 0.0001133842Method = EHM, EM cycle = 32, Max-Change = 0.0001042719Method = EHM, EM cycle = 33, Max-Change = 9.57953e-05
```

-   Parameter estimation results

``` r
### True item parameters 
item["b"]
#> [1] NA

### Estimated item parameters
Mod1$par_est["b"]
#> [1] NA

### Plotting
plot(item[,2], Mod1$par_est[,2], xlab = "true", ylab = "estimated")
abline(a=0,b=1)
```

<img src="man/figures/README-results-1.png" width="100%" style="display: block; margin: auto;" />

-   Latent distribution estimation result

``` r
plot_LD(Mod1)
```

<img src="man/figures/README-plotLD-1.png" width="100%" style="display: block; margin: auto;" />
