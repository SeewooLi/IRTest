#' Extract Log-Likelihood
#'
#' @param object A \code{IRTest}-class object from which a log-likelihood value is extracted.
#' @param ... Other arguments.
#'
#' @return Extracted loglikelihood.
#' @export
#'
logLik.IRTest <- function(object, ...){
  -object$logL/2
}
