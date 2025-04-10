#' Extract Model Coefficients
#'
#' @description
#' A generic function which extracts model coefficients from objects returned by modeling functions.
#'
#' @param object An object for which the extraction of model coefficients is meaningful.
#' @param complete A logical value indicating if the full coefficient vector should be returned.
#' @param ... Other arguments.
#'
#' @return Coefficients extracted from the model (\code{object}).
#' @export
#'
coef.IRTest <- function(object, complete = TRUE, ...){
  cf <- object$par_est
  if(inherits(object, "poly")){
    if(object$Options$model == "likert2"){
      cf <- cbind(
        exp(cf[,1]),
        t(apply(cf[,2:ncol(cf)], MARGIN = 1, FUN = cut_trans))
      )
      dn <- list(colnames(object$Options$data),c("nu", paste("b", 1:(ncol(cf)-1), sep="_")))
      dimnames(cf) <- dn
    }
  }
  if (complete)
    cf
  else cf[!is.na(cf)]
}

#' Extract Standard Errors of Model Coefficients
#'
#' @description
#' Standard errors of model coefficients calculated by using Fisher information functions.
#'
#' @param object An object for which the extraction of standard errors is meaningful.
#' @param complete A logical value indicating if the full standard-error vector should be returned.
#'
#' @return Standard errors extracted from the model (\code{object}).
#' @export
#'
coef_se <- function(object, complete = TRUE){
  cf <- object$se
  if (complete)
    cf
  else cf[!is.na(cf)]
}
