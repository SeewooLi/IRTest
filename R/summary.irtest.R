#' Summary of the results
#'
#' @description These functions summarize the outputs (e.g., convergence of the estimation algorithm, parameter estimates, AIC, etc.).
#'
#' @param object An \code{class == "irtest"} object obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param ... Other argument(s) passed on to summarize the results.
#'
#' @return A plot of estimated latent distribution.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' # Summary
#'
summary.irtest <- function(object, ...){
  NextMethod("summary.irtest")
}

summary.irtest.dich <- function(object, ...){

}

summary.irtest.poly <- function(object, ...){

}

summary.irtest.mix <- function(object, ...){

}
