#' ICC
#'
#' @param theta
#' @param a
#' @param b
#' @param c
#'
#' @return
#' @export
#'
#' @examples
#' P(0)

P <- function(theta,a=1,b,c=0){
  c+(1-c)*(1/(1+exp(-a*(theta-b))))
}
