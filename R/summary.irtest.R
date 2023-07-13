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

  # the number of parameters
  n_par <- data.frame(from_item = NULL, from_dist = NULL, total = NULL)

  n_par$from_item <- sum(object$Options$model %in% c(1, "1PL", "Rasch", "RASCH"))
  n_par$from_item <- npar$from_item + 2*sum(object$Options$model %in% c(2, "2PL"))
  n_par$from_item <- npar$from_item + 3*sum(object$Options$model %in% c(3, "3PL"))


  if(object$Options$latent_dist %in% c("Normal", "normal", "N")){
    n_par$from_dist <- 0
  }

  # Empirical histogram method
  if(object$Options$latent_dist=="EHM"){
    n_par$from_dist <- object$Options$q - 2
  }

  # Two-component normal mixture distribution
  if(object$Options$latent_dist %in% c("Mixture", "2NM")){
    n_par$from_dist <- 3
  }

  # Kernel density estimation method
  if(object$Options$latent_dist=="KDE"){
    n_par$from_dist <- 1
  }

  # Davidian curve method
  if(object$Options$latent_dist%in% c("DC", "Davidian")){
    n_par$from_dist <- object$Options$h
  }





}

summary.irtest.poly <- function(object, ...){

}

summary.irtest.mix <- function(object, ...){

}
