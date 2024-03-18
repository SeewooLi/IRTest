#' Plot of item response functions
#'
#' @description This function draws item response functions of an item of the fitted model.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, \code{IRTest_Cont}, or \code{IRTest_Mix}.
#' @param item.number A numeric value indicating the item number.
#' @param type A character string required if \code{inherits(x, c("mix")) == TRUE}.
#' It should be either \code{"d"} (dichotomous item) or \code{"p"} (polytomous item); \code{item.number=1, type="d"} indicates the first dichotomous item.
#'
#' @return This function returns a plot of item response functions.
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @export
#' @examples
#' \donttest{
#' # A preparation of dichotomous item response data
#'
#' data <- DataGeneration(N=500, nitem_D = 10)$data_D
#'
#' # Analysis
#'
#' M1 <- IRTest_Dich(data)
#'
#' # Plotting item response function
#'
#' plot_item(M1, item.number = 1)
#'}
plot_item <- function(x, item.number=1, type=NULL){
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
plot_item.dich <- function(x, item.number=1, type=NULL){
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
    ggplot2::ggplot(mapping = aes(x=seq(-6,6,0.1),y=probs))+
    ggplot2::geom_line()+
    ggplot2::lims(y=c(0,1))+
    ggplot2::labs(title = sprintf("Item %i", item.number), x = expression(theta), y = "probability", caption = sprintf("%s is applied.", model))+
    ggplot2::theme_bw()
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
plot_item.poly <- function(x, item.number=1, type=NULL){
  item_par <- x$par_est[item.number,]
  npar <- sum(!is.na(item_par))
  if(x$Option$model %in% c("PCM", "GPCM")){
    probs <- P_P(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
  } else if(x$Option$model %in% c("GRM")){
    probs <- P_G(seq(-6,6,0.1),a = item_par[1],b = item_par[-1])
  }
  category <- rep(paste("P", 1:npar-1, sep = ""), each=121)
  ppp <-
    ggplot2::ggplot(mapping = aes(x=rep(seq(-6,6,0.1), npar),y=as.vector(probs), colour=category))+
    ggplot2::geom_line()+
    ggplot2::lims(y=c(0,1))+
    ggplot2::labs(title = sprintf("Item %i", item.number), x = expression(theta), y = "probability", caption = sprintf("%s is applied.", x$Option$model))+
    ggplot2::theme_bw()
  return(ppp)
}

#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom stats qbeta
#'
plot_item.cont <- function(x, item.number=1, type=NULL){
  item_par <- x$par_est[item.number,]
  probs <- P(theta = seq(-6,6,0.1),
             a = item_par[1],
             b = item_par[2])
  lower.ci <- stats::qbeta(.025, item_par[3]*probs, (1-probs)*item_par[3])
  upper.ci <- stats::qbeta(.975, item_par[3]*probs, (1-probs)*item_par[3])
  p_df <- data.frame(response = c(probs, lower.ci, upper.ci),
                     theta = rep(seq(-6,6,0.1), 3),
                     type = rep(c("Mean", "C.I. (2.5%)", "C.I. (97.5%)"), each = 121))
  ppp <-
    ggplot2::ggplot()+
    ggplot2::geom_line(mapping = aes(x=p_df[p_df$type=="Mean","theta"],y=p_df[p_df$type=="Mean","response"]))+
    ggplot2::geom_line(mapping = aes(x=p_df[p_df$type!="Mean","theta"],y=p_df[p_df$type!="Mean","response"], group=p_df[p_df$type!="Mean","type"]), color="blue", linetype="dashed")+
    ggplot2::lims(y=c(0,1))+
    ggplot2::labs(
                  title = sprintf("Item %i", item.number),
                  x = expression(theta),
                  y = "response",
                  caption = "Continuous response item")+
    ggplot2::theme_bw()
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
plot_item.mix <- function(x, item.number=1, type="d"){
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
      ggplot2::ggplot(mapping = aes(x=seq(-6,6,0.1),y=probs))+
      ggplot2::geom_line()+
      ggplot2::lims(y=c(0,1))+
      ggplot2::labs(title = sprintf("Item %i", item.number), x = expression(theta), y = "probability", caption = sprintf("%s is applied.", model))+
      ggplot2::theme_bw()
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
      ggplot2::ggplot(mapping = aes(x=rep(seq(-6,6,0.1), npar),y=as.vector(probs), colour=category))+
      ggplot2::geom_line()+
      ggplot2::lims(y=c(0,1))+
      ggplot2::labs(title = sprintf("Item %i", item.number), x = expression(theta), y = "probability", caption = sprintf("%s is applied.", x$Option$model_P))+
      ggplot2::theme_bw()
  }
  return(ppp)
}



