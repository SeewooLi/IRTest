#' Plot of item response function
#'
#' @description This function draws a item response function of an item of the fitted model.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#' @param item.number A numeric value indicating the item number.
#' @param type Type of an item being either \code{"d"} (dichotomous item) or \code{"p"} (polytomous item); \code{item.number=1, type="d"} indicates the first dichotomous item.
#' This value will be used only when a model fit of mixed-format data is passed onto the function.
#'
#' @return This function returns a plot of item response function.
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @export
#'
plot_item <- function(x, item.number, type=NULL){
  UseMethod("plot_item", x)
}



#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#'
plot_item.dich <- function(x, item.number, type=NULL){
  item_par <- x$par_est[item.number,]
  probs <- P(theta = seq(-6,6,0.1),
             a = item_par[1],
             b = item_par[2],
             c = item_par[3])
  model <- x$Option$model[item.number]
  if(model %in% c(1, "1PL", "Rasch", "RASCH")){
    model <- "1PLM"
  } else if(model %in% c(2, "2PL")){
    model <- "2PLM"
  } else if(model %in% c(3, "3PL")){
    model <- "3PLM"
  }
  ppp <-
    ggplot(mapping = aes(x=seq(-6,6,0.1),y=probs))+
    geom_line()+
    lims(y=c(0,1))+
    labs(
      title = sprintf("Item %i", item.number),
      x = expression(theta),
      y = "probability",
      caption = sprintf("%s is applied.", model))+
    theme_bw()
  return(ppp)
}

#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#'
plot_item.poly <- function(x, item.number, type=NULL){
  item_par <- x$par_est[item.number,]
  npar <- sum(!is.na(item_par))
  if(x$Option$model %in% c("PCM", "GPCM")){
    probs <- P_P(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
  } else if(x$Option$model %in% c("GRM")){
    probs <- P_G(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
  }
  category <- rep(paste("P", 1:npar-1, sep = ""), each=121)
  ppp <-
    ggplot(mapping = aes(x=rep(seq(-6,6,0.1), npar),y=as.vector(probs), colour=category))+
    geom_line()+
    lims(y=c(0,1))+
    labs(
      title = sprintf("Item %i", item.number),
      x = expression(theta),
      y = "probability",
      caption = sprintf("%s is applied.", x$Option$model))+
    theme_bw()
  return(ppp)
}

#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#'
plot_item.mix <- function(x, item.number, type=NULL){
  if(type=="d"){
    item_par <- x$par_est[[1]][item.number,]
    probs <- P(theta = seq(-6,6,0.1),
               a = item_par[1],
               b = item_par[2],
               c = item_par[3])
    model <- x$Option$model_D[item.number]
    if(model %in% c(1, "1PL", "Rasch", "RASCH")){
      model <- "1PLM"
    } else if(model %in% c(2, "2PL")){
      model <- "2PLM"
    } else if(model %in% c(3, "3PL")){
      model <- "3PLM"
    }
    ppp <-
      ggplot(mapping = aes(x=seq(-6,6,0.1),y=probs))+
      geom_line()+
      lims(y=c(0,1))+
      labs(
        title = sprintf("Item %i", item.number),
        x = expression(theta),
        y = "probability",
        caption = sprintf("%s is applied.", model))+
      theme_bw()
  } else if(type=="p"){
    item_par <- x$par_est[[2]][item.number,]
    npar <- sum(!is.na(item_par))
    if(x$Option$model_P %in% c("PCM", "GPCM")){
      probs <- P_P(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
    } else if(x$Option$model_P %in% c("GRM")){
      probs <- P_G(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
    }
    category <- rep(paste("P", 1:npar-1, sep = ""), each=121)
    ppp <-
      ggplot(mapping = aes(x=rep(seq(-6,6,0.1), npar),y=as.vector(probs), colour=category))+
      geom_line()+
      lims(y=c(0,1))+
      labs(
        title = sprintf("Item %i", item.number),
        x = expression(theta),
        y = "probability",
        caption = sprintf("%s is applied.", x$Option$model_P))+
      theme_bw()
  }
  return(ppp)
}



