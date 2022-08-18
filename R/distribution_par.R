#' Recovering original parameters of two component normal mixture distribution
#' @description Recovering original parameters of two component normal mixture distribution from re-parameterized parameters.
#' @param prob ratio of two distributions.
#' @param d the constant.
#' @param sd_ratio ratio of standard deviation.
#' @param overallmean overall mean.
#' @param overallsd overall standard deviation.
#'
#' @return Parameters of two component normal mixture distribution.
#' m1 & s1 mean the mean and standard deviation of 1st distribution, m2 & s2  mean the mean and standard deviation of 2nd distribution.
#' @export distribution_par
#'
#' @examples
distribution_par <- function(prob = 0.5, d = 0, sd_ratio = 1, overallmean=0, overallsd=1){
  m1 <- -(1-prob)*d+overallmean
  m2 <- prob*d+overallmean
  s1 <- sqrt((overallsd^2-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
  s2 <- s1*sd_ratio
  return(c(m1,m2,s1,s2))
}
