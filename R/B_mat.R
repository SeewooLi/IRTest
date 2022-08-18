#' Title
#'
#' @return
#' @export
#'
#' @examples
B_mat <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(sqrt(eigen(mmat)$values))%*%t(eigen(mmat)$vectors)
  return(umat)
}
