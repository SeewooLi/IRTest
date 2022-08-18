#' Density for two component Normal Mixture Distributions
#' @description Density for two component Normal Mixture Distributions with means equal to m and standard deviations equal to s.
#' @param x vector of quantiles.
#' @param prob ratio of two distributions.
#' @param m vector of means.
#' @param s vector of standard deviations.
#'
#' @return Density
#' @export dist
#'
#' @examples
dist <- function(x, prob = 0.5, m = c(0,0), s = c(1,1)){
  prob*dnorm(x, m[1], s[1])+(1-prob)*dnorm(x, m[2], s[2])
}
