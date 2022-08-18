#' Density for re-parameterized two component Normal Mixture Distributions
#' @description Density for re-parameterizedtwo component Normal Mixture Distributions
#' @param x vector of quantiles.
#' @param prob ratio of two distributions.
#' @param d the constant.
#' @param sd_ratio ratio of standard deviation.
#' @param overallmean overall mean.
#' @param overallsd overall standard deviation.
#'
#' @return Density
#' @export dist2
#'
#' @examples
dist2 <- function(x, prob = 0.5, d = 0, sd_ratio = 1, overallmean=0, overallsd=1){
  m1 <- -(1-prob)*d+overallmean
  m2 <- prob*d+overallmean
  s1 <- sqrt((overallsd^2-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
  s2 <- s1*sd_ratio
  density <- prob*dnormal(x, m1, s1)+(1-prob)*dnormal(x, m2, s2)
  return(density)
}
