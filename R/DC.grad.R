#' Title
#'
#'
#' @return
#' @export
#'
#' @importFrom dcurver dc_grad
#' @examples
DC.grad <- function (phi, theta, freq) {
  -colSums(dcurver::dc_grad(theta, phi) * freq)
}
