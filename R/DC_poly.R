#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
DC_poly <- function(phi, theta, invB, cvec){
  h <- length(phi)
  m <- invB%*%cvec
  dcpoly <- as.vector(t(m)%*%sapply(theta,Z_vec,h=h))
  return(dcpoly)
}
