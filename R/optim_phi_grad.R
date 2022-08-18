#' Title
#'
#' @return
#' @export
#'
#' @examples
optim_phi_grad <- function(phi, hp){
  target <- as.vector(B_mat(hp)%*%rep(1,times=hp+1))
  cvec <- c_vec(phi)
  obj <- as.vector(4*(c_vec(phi)-target)^3%*%c_phi(phi,cvec))
  return(obj)
}
