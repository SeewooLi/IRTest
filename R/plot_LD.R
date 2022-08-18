#' Plot of Latent Distribution
#' @description Plotting of estimated latent density with IRTest
#' @param model 	parameter logistic model of item response theory.
#' 1 means one parameter logistic model, 2 means two parameter logistic model, 3 means three parameter logistic model.
#' @param from lower boundary of theta
#' @param to upper boundary of theta
#' @param add logical; if TRUE add to an already existing plot; if NA start a new plot taking the defaults for the limits and log-scaling of the x-axis from the previous plot. Taken as FALSE (with a warning if a different value is supplied) if no graphics device is open.
#' @return plot of latent distribution
#' @export plot_LD
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
