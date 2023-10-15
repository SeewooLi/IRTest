#' Re-parameterized two-component normal mixture distribution
#'
#' @description Probability density for the re-parameterized two-component normal mixture distribution.
#'
#' @param x A numeric vector. The location to evaluate the density function.
#' @param prob A numeric value of \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees belonging to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).
#' @param d A numeric value of \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
#' where \eqn{\mu_1} and \eqn{\mu_2} are the estimated mean of the first and second Gaussian component, respectively.
#' And \eqn{\bar{\sigma}} is the overall standard deviation of the latent distribution (Li, 2021).
#' Without loss of generality, \eqn{\mu_2 \ge \mu_1} is assumed, thus \eqn{\delta \ge 0}.
#' @param sd_ratio A numeric value of \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} and \eqn{\sigma_2} are the estimated standard deviation of the first and second Gaussian component, respectively (Li, 2021).
#' @param overallmean A numeric value of \eqn{\bar{\mu}} that determines the overall mean of two-component Gaussian mixture distribution.
#' @param overallsd A numeric value of \eqn{\bar{\sigma}} that determines the overall standard deviation of two-component Gaussian mixture distribution.
#'
#' @details
#'\describe{
#'\item{The overall mean and overall standard deviation obtained from original parameters;}{
#'1) Overall mean (\eqn{\bar{\mu}})
#'\deqn{\bar{\mu}=\pi\mu_1 + (1-\pi)\mu_2}
#'
#'2) Overall standard deviation (\eqn{\bar{\sigma}})
#'\deqn{\bar{\sigma}=\sqrt{\pi\sigma_{1}^{2}+(1-\pi)\sigma_{2}^{2}+\pi(1-\pi)(\mu_2-\mu_1)^2}}
#'}
#'}
#'
#'
#' @return
#' The evaluated probability density value(s).
#'
#' @export
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
#'
#' @examples
#' # Evaluated density
#' dnst <- dist2(seq(-6,6,.1), prob = 0.3, d = 1, sd_ratio=0.5)
#'
#' # Plot of the density
#' plot(seq(-6,6,.1), dnst)
#'
dist2 <- function(x, prob = 0.5, d = 0, sd_ratio = 1, overallmean=0, overallsd=1){
  m1 <- -(1-prob)*d*overallsd+overallmean
  m2 <- prob*d*overallsd+overallmean
  s1 <- sqrt(overallsd^2*(1-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
  s2 <- s1*sd_ratio
  density <- prob*dnormal(x, m1, s1)+(1-prob)*dnormal(x, m2, s2)
  return(density)
}
