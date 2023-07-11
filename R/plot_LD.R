#' Plotting the estimated latent distribution
#'
#' @description This function draws a plot of the estimated latent distribution (the distribution of the latent variable).
#'
#' @import ggplot2
#'
#' @param model An object obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param xlim A vector of length 2 which determines the range of the plot.
#' The default is \code{c(-6, 6)}.
#' @param ... Other argument(s) passed on to draw a plot of an estimated latent distribution.
#' If the distribution estimation method is the one using two-component normal mixture distribution (i.e., \code{latent_dist == "Mixture"} or \code{"2NM"}),
#' or the normal distribution is assumed (i.e., \code{latent_dist == "N"},  \code{"normal"}, or \code{"Normal"}), arguments are passed on to \code{\link{stat_function}}.
#' Otherwise, they are passed on to \code{\link{geom_line}}.
#'
#' @return A plot of estimated latent distribution.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' # Data generation and model fitting
#'
#' Alldata <- DataGeneration(seed = 1,
#'                           #model_D = rep(1, 10),
#'                           N=1000,
#'                           nitem_D = 0,
#'                           nitem_P = 8,
#'                           categ = rep(3:4,each = 4),
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
#' plot_LD(model=M1, xlim = c(-6, 6))
#'
plot_LD <- function(model, xlim = c(-6, 6), ...){
  if(model[["Options"]][["latent_dist"]] %in% c("Mixture", "2NM")){
    plt <- ggplot2::ggplot() +
      stat_function(
        fun = dist2,
        args = list(
          prob = model$prob,
          d=model$d,
          sd_ratio = model$sd_ratio
          ),
        ...
        )
  }else if(model[["Options"]][["latent_dist"]]%in% c("Normal", "normal", "N")){
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
        x=model$quad,
        y=model$Ak*(1/(model$quad[2]-model$quad[1]))
        )
      ) +
      geom_line(...)
  }

  return(
    plt +
      ylab("latent density") +
      xlab(expression(theta)) +
      scale_y_continuous(breaks = NULL)+
      lims(x=xlim)
  )
}
