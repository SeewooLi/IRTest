
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to **IRTest**!

*Please feel free to* [create an
issue](https://github.com/SeewooLi/IRTest/issues) *for bug reports or
potential improvements.*

<!-- badges: start -->

[![R-CMD-check](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/IRTest)](https://CRAN.R-project.org/package=IRTest)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/IRTest)](https://cranlogs.r-pkg.org/badges/grand-total/IRTest)
[![codecov](https://codecov.io/gh/SeewooLi/IRTest/branch/master/graph/badge.svg?token=N5RY2MYSM5)](https://app.codecov.io/gh/SeewooLi/IRTest)
<!-- badges: end -->

**IRTest** is a useful tool for $\mathcal{\color{red}{IRT}}$ (item
response theory) parameter $\mathcal{\color{red}{est}}\text{imation}$,
especially when the violation of normality assumption on latent
distribution is suspected.  
**IRTest** deals with uni-dimensional latent variable.  
In **IRTest**, including the conventional usage of Gaussian
distribution, several methods can be applied for estimation of latent
distribution:  
+ empirical histogram method,  
+ two-component Gaussian mixture distribution,  
+ Davidian curve,  
+ kernel density estimation.

## Installation

The CRAN version of **IRTest** can be installed on R-console with:

``` r
install.packages("IRTest")
```

For the development version, it can be installed on R-console with:

``` r
devtools::install_github("SeewooLi/IRTest")
```

## Functions

Followings are functions of **IRTest**.

- `IRTest_Dich` is the estimation function when all items are
  *dichotomously* scored.

- `IRTest_Poly` is the estimation function when all items are
  *polytomously* scored.

- `IRTest_Mix` is the estimation function for *a mixed-format test*, a
  test comprising both dichotomous item(s) and polytomous item(s).

- `item_fit` tests the statistical fit of all items individually.

- `plot_item` draws item response function(s) of an item.

- `DataGeneration` generates several objects that can be useful for
  computer simulation studies. Among these are starting values for the
  estimation algorithm and artificial item-response data that can be
  passed into `IRTest_Dich`, `IRTest_Poly`, or `IRTest_Mix`.

- `dist2` is a probability density function of two-component Gaussian
  mixture distribution.

- `original_par_2GM` converts re-parameterized parameters of
  two-component Gaussian mixture distribution into original parameters.

## Example

A simple simulation study for a Rasch model can be done in following
manners:

``` r
library(IRTest)
```

- Data generation

An artificial data of 1000 examinees and 20 items.

``` r
Alldata <- DataGeneration(seed = 1234,
                          model_D = rep(2, 40),
                          N=1000,
                          nitem_D = 40,
                          nitem_P = 0,
                          latent_dist = "2NM",
                          d = 1.664,
                          m=0,
                          s=2,
                          a_l = 0.5,
                          a_u = 0.51,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
theta <- Alldata$theta
```

- Analysis

For an illustrative purpose, empirical histogram method is used for the
estimation of latent distribution.

``` r
Mod1 <- IRTest_Dich(data = data,
                    model = rep("1PL", 40),
                    latent_dist = "KDE",
                    threshold = .001
                    )
```

- Summary of the result

``` r
summary(Mod1)
#> Convergence:  
#> Successfully converged below the threshold of 0.001 on 37th iterations. 
#> 
#> Model Fit:  
#>    deviance   44742.26 
#>         AIC   44824.26 
#>         BIC   45025.48 
#> 
#> The Number of Parameters:  
#>        item   40 
#>        dist   1 
#>       total   41 
#> 
#> The Number of Items:  
#> dichotomous   40 
#> polyotomous   0 
#> 
#> The Estimated Latent Distribution:  
#> method - KDE 
#> ----------------------------------------
#>                                           
#>                                           
#>                                           
#>                       . @ .               
#>           . .       @ @ @ @ @             
#>         @ @ @ @ @ @ @ @ @ @ @ @           
#>       . @ @ @ @ @ @ @ @ @ @ @ @ @         
#>     . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @       
#>     @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @     
#>   @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ . 
#> +---------+---------+---------+---------+
#> -2        -1        0         1         2
```

- Parameter estimation results

``` r
colnames(item) <- c("a", "b", "c")

knitr::kables(
  list(
    ### True item parameters 
    knitr::kable(item, format='simple', caption = "True item parameters", digits = 2)%>%
  kableExtra::kable_styling(font_size = 4),

    ### Estimated item parameters
    knitr::kable(Mod1$par_est, format='simple', caption = "Estimated item parameters", digits = 2)%>%
  kableExtra::kable_styling(font_size = 4)
  )
)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

|    a |     b |   c |
|-----:|------:|----:|
| 0.50 | -0.60 |   0 |
| 0.51 | -2.00 |   0 |
| 0.50 | -1.80 |   0 |
| 0.51 | -1.36 |   0 |
| 0.50 | -0.28 |   0 |
| 0.51 | -2.06 |   0 |
| 0.50 | -1.04 |   0 |
| 0.50 |  2.50 |   0 |
| 0.50 |  3.42 |   0 |
| 0.50 |  3.62 |   0 |
| 0.50 | -2.78 |   0 |
| 0.50 | -0.04 |   0 |
| 0.50 |  1.24 |   0 |
| 0.51 | -0.12 |   0 |
| 0.50 | -2.16 |   0 |
| 0.50 |  3.76 |   0 |
| 0.51 | -1.28 |   0 |
| 0.50 |  3.56 |   0 |
| 0.50 |  2.06 |   0 |
| 0.51 | -1.40 |   0 |
| 0.51 | -3.20 |   0 |
| 0.50 |  0.54 |   0 |
| 0.50 |  1.94 |   0 |
| 0.50 |  1.14 |   0 |
| 0.50 | -2.44 |   0 |
| 0.50 |  2.08 |   0 |
| 0.50 | -0.34 |   0 |
| 0.50 |  3.78 |   0 |
| 0.51 |  0.40 |   0 |
| 0.51 | -3.04 |   0 |
| 0.50 | -1.30 |   0 |
| 0.51 | -1.12 |   0 |
| 0.51 | -1.56 |   0 |
| 0.50 |  0.66 |   0 |
| 0.50 |  1.60 |   0 |
| 0.51 | -0.42 |   0 |
| 0.51 | -1.40 |   0 |
| 0.50 |  0.96 |   0 |
| 0.50 | -0.84 |   0 |
| 0.50 | -0.96 |   0 |

True item parameters

</td>
<td>

|   a |     b |   c |
|----:|------:|----:|
|   1 | -0.35 |   0 |
|   1 | -1.15 |   0 |
|   1 | -0.91 |   0 |
|   1 | -0.57 |   0 |
|   1 | -0.16 |   0 |
|   1 | -1.18 |   0 |
|   1 | -0.46 |   0 |
|   1 |  1.22 |   0 |
|   1 |  1.85 |   0 |
|   1 |  1.78 |   0 |
|   1 | -1.27 |   0 |
|   1 | -0.12 |   0 |
|   1 |  0.70 |   0 |
|   1 | -0.03 |   0 |
|   1 | -1.23 |   0 |
|   1 |  1.81 |   0 |
|   1 | -0.55 |   0 |
|   1 |  1.92 |   0 |
|   1 |  1.11 |   0 |
|   1 | -0.67 |   0 |
|   1 | -1.62 |   0 |
|   1 |  0.24 |   0 |
|   1 |  1.06 |   0 |
|   1 |  0.66 |   0 |
|   1 | -1.16 |   0 |
|   1 |  1.12 |   0 |
|   1 | -0.09 |   0 |
|   1 |  1.88 |   0 |
|   1 |  0.32 |   0 |
|   1 | -1.50 |   0 |
|   1 | -0.74 |   0 |
|   1 | -0.60 |   0 |
|   1 | -0.94 |   0 |
|   1 |  0.31 |   0 |
|   1 |  0.76 |   0 |
|   1 | -0.20 |   0 |
|   1 | -0.80 |   0 |
|   1 |  0.50 |   0 |
|   1 | -0.44 |   0 |
|   1 | -0.61 |   0 |

Estimated item parameters

</td>
</tr>
</tbody>
</table>

``` r


### Plotting
par(mfrow=c(1,2))
plot(item[,2], Mod1$par_est[,2], xlab = "true", ylab = "estimated", main = "item parameters")
abline(a=0,b=.5)
plot(theta, Mod1$theta, xlab = "true", ylab = "estimated", main = "ability parameters")
abline(a=0,b=.5)
```

<img src="man/figures/README-results-1.png" width="100%" style="display: block; margin: auto;" />

- The result of latent distribution estimation

``` r
plot(Mod1, mapping = aes(colour="Estimated"), linewidth = 1) +
  lims(
    y = c(0, .75)
  )+
  geom_line(
    mapping=aes(
      x=seq(-6,6,length=121), 
      y=dist2(
        seq(-6,6,length=121),
        prob = .3, 
        d=1.664, 
        sd_ratio = 2
        ), 
      colour="True"),
    linewidth = 1)+
  labs(
    title="The estimated latent density using 'EHM'", colour= "Type"
    )+
  theme_bw()
```

<img src="man/figures/README-plotLD-1.png" width="100%" style="display: block; margin: auto;" />

- Posterior distributions for the examinees

Each examinee’s posterior distribution is identified in the E-step of
the estimation algorithm (i.e., EM algorithm). Posterior distributions
can be found in `Mod1$Pk`.

``` r
set.seed(1)
selected_examinees <- sample(1:1000,6)
post_sample <- 
  data.frame(
    X = rep(seq(-6,6, length.out=121),6), 
    prior = rep(Mod1$Ak/(Mod1$quad[2]-Mod1$quad[1]), 6),
    posterior = 10*c(t(Mod1$Pk[selected_examinees,])), 
    ID = rep(paste("examinee", selected_examinees), each=121)
    )

ggplot(data=post_sample, mapping=aes(x=X))+
  geom_line(mapping=aes(y=posterior, group=ID, color='Posterior'))+
  geom_line(mapping=aes(y=prior, group=ID, color='Prior'))+
  labs(title="Posterior densities for selected examinees", x=expression(theta), y='density')+
  facet_wrap(~ID, ncol=2)+
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

- Item fit

``` r
item_fit(Mod1)
#>         stat df p.value
#> 1   7.551248  8  0.4785
#> 2  20.086276  8  0.0100
#> 3   6.021773  8  0.6448
#> 4   2.294674  8  0.9706
#> 5   4.907049  8  0.7675
#> 6  10.188878  8  0.2520
#> 7  12.503459  8  0.1301
#> 8  11.453869  8  0.1773
#> 9  12.567177  8  0.1276
#> 10 13.406883  8  0.0986
#> 11  6.425834  8  0.5996
#> 12  4.007489  8  0.8564
#> 13 13.663913  8  0.0910
#> 14 13.965909  8  0.0827
#> 15  6.999904  8  0.5366
#> 16  5.229552  8  0.7328
#> 17 13.555818  8  0.0941
#> 18 14.321294  8  0.0738
#> 19 11.365245  8  0.1819
#> 20  8.371562  8  0.3980
#> 21  9.785573  8  0.2804
#> 22  7.197640  8  0.5155
#> 23  5.620715  8  0.6896
#> 24  5.266007  8  0.7288
#> 25 14.996512  8  0.0592
#> 26  9.125490  8  0.3318
#> 27 10.504135  8  0.2314
#> 28  7.334369  8  0.5010
#> 29  6.498262  8  0.5916
#> 30 11.363527  8  0.1819
#> 31 13.603710  8  0.0927
#> 32 10.800962  8  0.2132
#> 33  4.150455  8  0.8433
#> 34 11.259085  8  0.1874
#> 35  9.521711  8  0.3002
#> 36  7.469010  8  0.4870
#> 37  3.758070  8  0.8783
#> 38 18.895604  8  0.0154
#> 39 19.161536  8  0.0140
#> 40  8.221141  8  0.4122
```

- Item response function

``` r
# Item response function of Item 1
plot_item(Mod1,1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
