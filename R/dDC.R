#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
dDC <- function(phi, theta){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  densDC <- (DC_poly(phi, theta, invB, cvec))^2*dnormal(theta)
  return(densDC)
}
