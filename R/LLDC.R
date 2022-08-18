#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
LLDC <- function(phi, theta, freq){
  -freq%*%log(dDC(phi, theta))
}
