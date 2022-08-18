#' Title
#'
#' @return
#' @export
#'
#' @examples
B_mat2 <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(svd(mmat)$d^.25)%*%t(svd(mmat)$v)
  return(umat)
}
