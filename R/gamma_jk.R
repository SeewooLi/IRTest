#' Title
#'
#' @return
#' @export
#'
#' @examples
gamma_jk <- function(i){
  Pk[,i] <- exp(logLikeli(item = item, data = data, theta = Xk[i]))*Ak[i]
}
