#' Plotting the estimated latent distribution
#'
#' @description This function draws a plot of the estimated latent distribution (the prior distribution of the latent variable).
#'
#' @param model An object obtained from
#' @param from
#' @param to
#' @param add
#'
#' @return
#' @export
#'
#' @examples
plot_LD <- function(model, from = -6, to=6, add = F){
  if(model[["Options"]][["latent_dist"]]=="Mixture"){
    curve(dist2(x,prob = model$prob, d=model$d, sd_ratio = model$sd_ratio),
          from = from, to=to, add = add, ylab="latent density", xlab=expression(theta))
  } else {
    if(model[["Options"]][["latent_dist"]]=="Normal"){
      curve(dnormal(x),
            from = from, to=to, add = add, ylab="latent density", xlab=expression(theta))
    }else{
      plot(model$quad,model$Ak*(1/(model$quad[2]-model$quad[1])),
           type = "l", ylab="latent density", xlab=expression(theta))
    }
  }
}
