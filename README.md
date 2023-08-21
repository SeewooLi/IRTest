
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
+ kernel density estimation,  
+ log-linear smoothing.

## Installation

The CRAN version of **IRTest** can be installed on R-console with:

    install.packages("IRTest")

For the development version, it can be installed on R-console with:

    devtools::install_github("SeewooLi/IRTest")

## Functions

Followings are functions of **IRTest**.

- `IRTest_Dich` is the estimation function when all items are
  *dichotomously* scored.

- `IRTest_Poly` is the estimation function when all items are
  *polytomously* scored.

- `IRTest_Mix` is the estimation function for *a mixed-format test*, a
  test comprising both dichotomous item(s) and polytomous item(s).

- `item_fit` tests the statistical fit of all items individually.

- `inform_f_item` calculates the information value(s) of an item.

- `inform_f_test` calculates the information value(s) of a test.

- `plot_item` draws item response function(s) of an item.

- `reliability` calculates marginal reliability coefficient of IRT.

- `DataGeneration` generates several objects that can be useful for
  computer simulation studies. Among these are simulated item
  parameters, ability parameters and the corresponding item-response
  data.

- `dist2` is a probability density function of two-component Gaussian
  mixture distribution.

- `original_par_2GM` converts re-parameterized parameters of
  two-component Gaussian mixture distribution into original parameters.

- `cat_clps` recommends category collapsing based on item parameters
  (or, equivalently, item response functions).

- `recategorize` implements the category collapsing.

## Example

A simple simulation study for a 2PL model can be done in following
manners:

``` r
library(IRTest)
```

- Data generation

An artificial data of 1000 examinees and 20 items.

``` r
Alldata <- DataGeneration(seed = 1234,
                          model_D = rep(2,20),
                          N=1000,
                          nitem_D = 20,
                          latent_dist = "2NM",
                          m=0,
                          s=1,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
theta <- Alldata$theta
```

- Analysis

For an illustrative purpose, kernel density estimation (KDE) method is
used for the estimation of latent distribution.

``` r
Mod1 <- 
  IRTest_Dich(
    data = data,
    latent_dist = "DC",
    h=7
    )
```

- Summary of the result

``` r
summary(Mod1)
#> Convergence:  
#> Successfully converged below the threshold of 1e-04 on 40th iterations. 
#> 
#> Model Fit:  
#>    deviance   18390.3 
#>         AIC   18484.3 
#>         BIC   18714.96 
#> 
#> The Number of Parameters:  
#>        item   40 
#>        dist   7 
#>       total   47 
#> 
#> The Number of Items:  
#> dichotomous   20 
#> polyotomous   0 
#> 
#> The Estimated Latent Distribution:  
#> method - DC 
#> ----------------------------------------
#>                                           
#>                                           
#>                                           
#>                       . . @ .             
#>                 . . @ @ @ @ @ .           
#>           . @ @ @ @ @ @ @ @ @ @ .         
#>         @ @ @ @ @ @ @ @ @ @ @ @ @         
#>       @ @ @ @ @ @ @ @ @ @ @ @ @ @ @       
#>     @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ .     
#> . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ .   
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
| 1.62 | -0.30 |   0 |
| 1.22 | -1.00 |   0 |
| 1.73 | -0.90 |   0 |
| 1.36 | -0.68 |   0 |
| 2.19 | -0.14 |   0 |
| 1.17 | -1.03 |   0 |
| 1.80 | -0.52 |   0 |
| 1.90 |  1.25 |   0 |
| 1.06 |  1.71 |   0 |
| 0.90 |  1.81 |   0 |
| 2.12 | -1.39 |   0 |
| 2.23 | -0.02 |   0 |
| 0.87 |  0.62 |   0 |
| 2.35 | -0.06 |   0 |
| 1.50 | -1.08 |   0 |
| 2.39 |  1.88 |   0 |
| 2.25 | -0.64 |   0 |
| 2.25 |  1.78 |   0 |
| 1.10 |  1.03 |   0 |
| 1.65 | -0.70 |   0 |

True item parameters

</td>
<td>

|    a |     b |   c |
|-----:|------:|----:|
| 1.50 | -0.26 |   0 |
| 1.25 | -0.94 |   0 |
| 1.81 | -0.81 |   0 |
| 1.44 | -0.56 |   0 |
| 1.85 | -0.14 |   0 |
| 1.22 | -0.96 |   0 |
| 2.03 | -0.46 |   0 |
| 2.00 |  1.35 |   0 |
| 1.19 |  1.46 |   0 |
| 1.11 |  1.61 |   0 |
| 1.86 | -1.43 |   0 |
| 2.24 | -0.04 |   0 |
| 0.77 |  0.71 |   0 |
| 2.25 |  0.00 |   0 |
| 1.57 | -1.07 |   0 |
| 2.58 |  1.83 |   0 |
| 2.21 | -0.65 |   0 |
| 2.73 |  1.64 |   0 |
| 1.22 |  0.92 |   0 |
| 1.94 | -0.68 |   0 |

Estimated item parameters

</td>
</tr>
</tbody>
</table>

``` r


### Plotting
par(mfrow=c(1,3))
plot(item[,1], Mod1$par_est[,1], xlab = "true", ylab = "estimated", main = "item discrimination parameters")
abline(a=0,b=1)
plot(item[,2], Mod1$par_est[,2], xlab = "true", ylab = "estimated", main = "item difficulty parameters")
abline(a=0,b=1)
plot(theta, Mod1$theta, xlab = "true", ylab = "estimated", main = "ability parameters")
abline(a=0,b=1)
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
#> 1  11.240810  7  0.1285
#> 2  10.438912  7  0.1650
#> 3  19.624820  7  0.0064
#> 4   8.723559  7  0.2731
#> 5   7.915895  7  0.3401
#> 6  13.248208  7  0.0663
#> 7   4.677790  7  0.6992
#> 8  15.438317  7  0.0308
#> 9   9.976055  7  0.1899
#> 10 15.982290  7  0.0253
#> 11  5.272871  7  0.6267
#> 12  9.582016  7  0.2135
#> 13  7.152190  7  0.4132
#> 14 24.813763  7  0.0008
#> 15 14.465369  7  0.0435
#> 16 19.649980  7  0.0064
#> 17  8.145667  7  0.3199
#> 18 66.917846  7  0.0000
#> 19 18.906368  7  0.0085
#> 20 10.737711  7  0.1505
```

- Item response function

``` r
# Item response function of Item 1
p1 <- plot_item(Mod1,10)
p2 <- plot_item(Mod1,11)
p3 <- plot_item(Mod1,13)
p4 <- plot_item(Mod1,16)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

- Reliability

``` r
reliability(Mod1)
#> $rel.summed.score.test
#> test reliability 
#>        0.8806951 
#> 
#> $rel.summed.score.item
#>         1         2         3         4         5         6         7         8 
#> 0.3081466 0.2234867 0.3538156 0.2862139 0.3887192 0.2164527 0.4200541 0.2669627 
#>         9        10        11        12        13        14        15        16 
#> 0.1549640 0.1349175 0.2907841 0.4644806 0.1090059 0.4638354 0.2823802 0.2295097 
#>        17        18        19        20 
#> 0.4421841 0.2790354 0.1980922 0.3894833
```

- Test information function

``` r
ggplot()+
  stat_function(
    fun = inform_f_test,
    args = list(Mod1)
  )+
  lims(x=c(-6,6))+
  labs(title="Test information function", x=expression(theta), y='information')+
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
