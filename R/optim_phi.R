#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
optim_phi <- function(phi, hp){
  target <- as.vector(B_mat(hp)%*%rep(1,times=hp+1))
  obj <- sum((c_vec(phi)-target)^4)
  return(obj)
}
