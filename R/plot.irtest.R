#' Plotting the estimated latent distribution
#'
#' @description This function draws a plot of the estimated latent distribution (the population distribution of the latent variable).
#'
#' @import ggplot2
#'
#' @param x An \code{class == "irtest"} object obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param ... Other argument(s) passed on to draw a plot of an estimated latent distribution.
#' Arguments are passed on to \code{\link{stat_function}}, if the distribution estimation method is the one using two-component normal mixture distribution (i.e., \code{latent_dist == "Mixture"} or \code{"2NM"})
#' or the normal distribution (i.e., \code{latent_dist == "N"},  \code{"normal"}, or \code{"Normal"}).
#' Otherwise, they are passed on to \code{\link{geom_line}}.
#'
#' @return A plot of estimated latent distribution.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' \dontrun{
#' # Data generation and model fitting
#'
#' Alldata <- DataGeneration(seed = 1,
#'                           #model_D = rep(1, 10),
#'                           N=1000,
#'                           nitem_D = 0,
#'                           nitem_P = 8,
#'                           categ = rep(3:4,each = 4),
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_P
#' item <- Alldata$item_P
#' initialitem <- Alldata$initialitem_P
#' theta <- Alldata$theta
#'
#' M1 <- IRTest_Poly(initialitem = initialitem,
#'                   data = data,
#'                   model = "GPCM",
#'                   latent_dist = "Mixture",
#'                   max_iter = 200,
#'                   threshold = .001,
#'                   )
#'
#'
#' # Plotting the latent distribution
#'
#' plot(x=M1, linewidth = 1, color = 'red') +
#'   ggplot2::lims(x = c(-6, 6), y = c(0, .5))
#'}
plot.irtest <- function(x, ...){
  if(x[["Options"]][["latent_dist"]] %in% c("Mixture", "2NM")){
    plt <- ggplot2::ggplot() +
      stat_function(
        fun = dist2,
        args = list(
          prob = x$density_par$prob,
          d=x$density_par$d,
          sd_ratio = x$density_par$sd_ratio
          ),
        ...
        )
  }else if(x[["Options"]][["latent_dist"]]%in% c("Normal", "normal", "N")){
    message('Latent distribution is always normal distribution if "latent_dist = "Normal""')
    plt <- ggplot2::ggplot() +
      stat_function(
        fun = dnormal,
        args = list(mean=0, sd=1),
        ...
        )
  }else{
    plt <- ggplot2::ggplot(
      mapping=aes(
        x=x$quad,
        y=x$Ak*(1/(x$quad[2]-x$quad[1]))
        )
      ) +
      geom_line(...)
  }

  return(
    plt +
      ylab("latent density") +
      xlab(expression(theta)) +
      lims(x=c(-6,6))
    )
}
