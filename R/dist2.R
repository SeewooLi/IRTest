#' Re-parameterized two-component normal mixture distribution
#'
#' @description Probability density for the re-parameterized two-component normal mixture distribution.
#'
#' @param x
#' @param prob
#' @param d
#' @param sd_ratio
#' @param overallmean
#' @param overallsd
#'
#' @return
#' @export
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
