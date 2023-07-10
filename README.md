
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to **IRTest**!

*Please feel free to* [create an
issue](https://github.com/SeewooLi/IRTest/issues) *for bug reports or
potential improvements.*

<!-- badges: start -->

[![R-CMD-check](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SeewooLi/IRTest/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/IRTest)](https://CRAN.R-project.org/package=IRTest)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/IRTest)]()
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

**IRTest** can be installed on R-console with:

``` r
install.packages("IRTest")
```

## Functions

Followings are functions of **IRTest**.

- `IRTest_Dich` is the estimation function when all items are
  *dichotomously* scored.

- `IRTest_Poly` is the estimation function when all items are
  *polytomously* scored.

- `IRTest_Mix` is the estimation function for *a mixed-format test*, a
  test comprising both dichotomous item(s) and polytomous item(s).

- `DataGeneration` generates several objects that can be useful for
  computer simulation studies. Among these are starting values for the
  estimation algorithm and artificial item-response data that can be
  passed into `IRTest_Dich`, `IRTest_Poly`, or `IRTest_Mix`.

- `plot_LD` draws a plot of the estimated latent distribution.

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
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(1, 20),
                          N=1000,
                          nitem_D = 20,
                          nitem_P = 0,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
initialitem <- Alldata$initialitem_D
theta <- Alldata$theta
```

- Analysis

For an illustrative purpose, empirical histogram method is used for the
estimation of latent distribution.

``` r
Mod1 <- IRTest_Dich(initialitem = initialitem,
                    data = data,
                    model = rep("1PL", 20),
                    latent_dist = "EHM",
                    threshold = .001
                    )
```

- Parameter estimation results

``` r
colnames(item) <- c("a", "b", "c")

knitr::kables(
  list(
    ### True item parameters 
    knitr::kable(item, format='simple', caption = "True item parameters", digits = 2),

    ### Estimated item parameters
    knitr::kable(Mod1$par_est, format='simple', caption = "Estimated item parameters", digits = 2)
  )
)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

|   a |     b |   c |
|----:|------:|----:|
|   1 | -0.96 |   0 |
|   1 |  1.04 |   0 |
|   1 |  0.47 |   0 |
|   1 | -0.16 |   0 |
|   1 | -0.81 |   0 |
|   1 | -0.40 |   0 |
|   1 |  0.82 |   0 |
|   1 | -0.37 |   0 |
|   1 | -1.11 |   0 |
|   1 |  0.50 |   0 |
|   1 | -0.97 |   0 |
|   1 | -1.05 |   0 |
|   1 |  0.02 |   0 |
|   1 |  1.32 |   0 |
|   1 | -0.50 |   0 |
|   1 |  0.18 |   0 |
|   1 | -1.39 |   0 |
|   1 |  0.59 |   0 |
|   1 | -0.58 |   0 |
|   1 | -1.59 |   0 |

True item parameters

</td>
<td>

|   a |     b |   c |
|----:|------:|----:|
|   1 | -0.82 |   0 |
|   1 |  0.95 |   0 |
|   1 |  0.47 |   0 |
|   1 | -0.06 |   0 |
|   1 | -0.85 |   0 |
|   1 | -0.43 |   0 |
|   1 |  0.89 |   0 |
|   1 | -0.32 |   0 |
|   1 | -1.17 |   0 |
|   1 |  0.54 |   0 |
|   1 | -1.07 |   0 |
|   1 | -1.16 |   0 |
|   1 |  0.07 |   0 |
|   1 |  1.25 |   0 |
|   1 | -0.43 |   0 |
|   1 |  0.20 |   0 |
|   1 | -1.38 |   0 |
|   1 |  0.60 |   0 |
|   1 | -0.75 |   0 |
|   1 | -1.70 |   0 |

Estimated item parameters

</td>
</tr>
</tbody>
</table>

``` r


### Plotting
par(mfrow=c(1,2))
plot(item[,2], Mod1$par_est[,2], xlab = "true", ylab = "estimated", main = "item parameters")
abline(a=0,b=1)
plot(theta, Mod1$theta, xlab = "true", ylab = "estimated", main = "ability parameters")
abline(a=0,b=1)
```

<img src="man/figures/README-results-1.png" width="100%" style="display: block; margin: auto;" />

- The result of latent distribution estimation

``` r
plot_LD(Mod1)+
  geom_line(mapping = aes(colour="Estimated"), size = 1)+
  geom_line(mapping=aes(x=seq(-6,6,length=121), 
                        y=dist2(seq(-6,6,length=121),prob = .3, d=1.664, sd_ratio = 2), 
                        colour="True"),
                        size = 1)+
  ylim(c(0,0.75))+
  labs(title="The estimated latent density using 'EHM'", colour= "Type")+
  theme_bw()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```

<img src="man/figures/README-plotLD-1.png" width="100%" style="display: block; margin: auto;" />

- Posterior distributions for the examinees

Each examinee’s posterior distribution is identified in the E-step of
the estimation algorithm (i.e., EM algorithm). Posterior distributions
can be found in `Mod1$Pk`.

``` r
set.seed(1)
selected_examinees <- sample(1:1000,6)
post_sample <- data.frame(X=rep(seq(-6,6, length.out=121),6), 
                          prior = rep(dist2(seq(-6,6,length=121),prob = .3, d=1.664, sd_ratio = 2), 6),
                          posterior = 10*c(t(Mod1$Pk[selected_examinees,])), 
                          ID=rep(paste("examinee", selected_examinees), each=121))
ggplot(data=post_sample, mapping=aes(x=X))+
  geom_line(mapping=aes(y=posterior, group=ID, color='Posterior'))+
  geom_line(mapping=aes(y=prior, group=ID, color='Prior'))+
  labs(title="Posterior densities for selected examinees", x=expression(theta), y='density')+
  facet_wrap(~ID, ncol=2)+
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
