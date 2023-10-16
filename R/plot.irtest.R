#' Plot of the estimated latent distribution
#'
#' @description This function draws a plot of the estimated latent distribution (the population distribution of the latent variable).
#'
#' @import ggplot2
#'
#' @param x An object of \code{"IRTest"}-class obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param ... Other aesthetic argument(s) for drawing the plot.
#' Arguments are passed on to \code{\link{stat_function}}, if the distribution estimation method is 2NM, KDE, or DC.
#' Otherwise, they are passed on to \code{\link{geom_line}}.
#'
#' @return A plot of estimated latent distribution.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' \donttest{
#' # Data generation and model fitting
#'
#' data <- DataGeneration(N=1000,
#'                        nitem_D = 15,
#'                        latent_dist = "2NM",
#'                        d = 1.664,
#'                        sd_ratio = 2,
#'                        prob = 0.3)$data_D
#'
#' M1 <- IRTest_Dich(data = data, latent_dist = "KDE")
#'
#' # Plotting the latent distribution
#'
#' plot(x = M1, linewidth = 1, color = 'red') +
#'   ggplot2::lims(x = c(-6, 6), y = c(0, .5))
#'}
plot.IRTest <- function(x, ...){
  if(x[["Options"]][["latent_dist"]] %in% c("EHM", "LLS")){
    plt <- ggplot2::ggplot(
      mapping = aes(
        x = x$quad,
        y = x$Ak*(1/(x$quad[2]-x$quad[1]))
      )
    ) +
    geom_line(...)
  }else{
    plt <- ggplot2::ggplot() +
      stat_function(
        fun = latent_distribution,
        args = list(x),
        ...
      )
  }

  return(
    plt +
      ylab("latent density") +
      xlab(expression(theta)) +
      lims(x=c(-6,6))
    )
}
