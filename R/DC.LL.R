#' Title
#'
#' @return
#' @export
#' @importFrom dcurver ddc
#' @examples
DC.LL <- function (phi, theta, freq) {
  LL <- sum(freq * log(dcurver::ddc(theta, phi)))
  -LL
}
