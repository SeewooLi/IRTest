#' Latent density function
#'
#' @description
#' Density function of the estimated latent distribution with mean and standard deviation equal to 0 and 1, respectively.
#'
#' @param x A numeric vector. Value(s) in the \eqn{theta} scale to evaluate the PDF.
#' @param model.fit An object returned from an estimation function.
#'
#' @return
#' The evaluated values of PDF, a length of which equals to that of \code{x}.
#'
#' @export
#' @import ggplot2
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 stat_function
#' @importFrom ggplot2 lims
#' @examples
#' # Data generation and model fitting
#' data <- DataGeneration(N=1000,
#'                        nitem_P = 10,
#'                        latent_dist = "2NM",
#'                        d = 1.664,
#'                        sd_ratio = 2,
#'                        prob = 0.3)$data_P
#'
#' M1 <- IRTest_Poly(data = data, latent_dist = "KDE")
#'
#'
#' # Plotting the latent distribution
#' ggplot2::ggplot()+
#'   ggplot2::stat_function(fun=latent_distribution, args=list(M1))+
#'   ggplot2::lims(x=c(-6,6), y=c(0,0.5))
#'
#'
latent_distribution <- function(x, model.fit){
  dlatent <- NULL
  if(model.fit$Options$latent_dist=="KDE"){
    dlatent <- outer(x, model.fit$quad, FUN = dnormal, sd=model.fit$density_par[1])%*%model.fit$Ak
  } else if(model.fit$Options$latent_dist%in%c("DC", "Davidian")){
    dlatent <- dcurver::ddc(x, model.fit$density_par)
  } else if(model.fit$Options$latent_dist %in% c("Mixture", "2NM")){
    dlatent <- dist2(x,
                     prob = model.fit$density_par$prob,
                     d = model.fit$density_par$d,
                     sd_ratio = model.fit$density_par$sd_ratio)
  } else if(model.fit$Options$latent_dist %in% c("Normal", "normal", "N")){
    dlatent <- dnorm(x)
  } else{
    message('For `latent_dist = "EHM"` and `latent_dist = "LLS"`, a PMF is estimated instead of PDF. Approach the estimated PMF from the `model.fit$Ak`.')
  }
  return(dlatent)
}


