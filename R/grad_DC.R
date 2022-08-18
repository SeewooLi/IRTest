#' Title
#'
#' @return
#' @export
#'
#' @examples
grad_DC <- function(phi, theta, freq){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  matalg <- t(c_phi(phi, cvec))%*%t(invB)%*%sapply(c(theta),Z_vec,h=h)
  dv1 <- -(2*freq/DC_poly(phi, theta, invB, cvec))%*%t(matalg)
  return(dv1)
}
