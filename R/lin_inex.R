#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
lin_inex <- function(qp, qh, range, rule=2){
  q <- length(qp)
  m <- qp%*%qh
  s <- (qp-c(m))^2%*%qh
  qp <- (qp-as.vector(m))/sqrt(as.vector(s))
  ap <- approx(qp, y = qh, xout = seq(range[1],range[2],length=q),
               method = "linear", rule=rule)
  return(list(qp=ap$x, qh=ap$y/sum(ap$y)))
}
