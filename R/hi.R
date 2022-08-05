#' hi
#' @param x name to say hi to
#' @return a string of greeting
#' @export
#' @importFrom dcurver ddc
#' @examples
#' hi("gap")
hi <- function(x){
  print(paste("Say, hi to", x, "~.", dcurver::ddc(0,0)))
}
