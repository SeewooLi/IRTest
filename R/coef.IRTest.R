#' Extract Model Coefficients
#'
#' @description
#' A generic function which extracts model coefficients from objects returned by modeling functions.
#'
#' @param object An object for which the extraction of model coefficients is meaningful.
#' @param complete A logical value indicating if the full coefficient vector should be returned.
#' @param ... Other arguments.
#'
#' @return Coefficients extracted from the model \code{object} object.
#' @export
#'
coef.IRTest <- function(object, complete = TRUE, ...){
  cf <- object$par_est
  if (complete)
    cf
  else cf[!is.na(cf)]
}
