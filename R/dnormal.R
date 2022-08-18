#' Density for the normal distribution
#' @description Density for the normal distribution with mean equal to mean and standard deviation equal to sd.
#' @param x vector of quantiles.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#'
#' @return Density
#' @export dnormal
#'
#' @examples
dnormal <- function(x, mean=0, sd=1){
  (2*pi)^(-0.5)/sd*exp(-(x-c(mean))^2/(2*sd^2))
}
