#' Title
#'
#' @return
#' @export
#'
#' @examples
c_vec <- function(phi){
  h <- length(phi)
  cvec <- numeric(h+1)
  cvec[1] <- sin(phi[1])
  if(h>1){
    for(i in 2:h){
      cvec[i] <- prod(cos(phi[1:(i-1)]))*sin(phi[i])
    }
  }
  cvec[h+1] <- prod(cos(phi))
  return(cvec)
}
