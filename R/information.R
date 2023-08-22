#' Item information function
#'
#' @param x A vector of \eqn{\theta} value(s).
#' @param item.matrix A matrix of item parameters.
#' @param item A numeric value indicating an item.
#' If \eqn{n} is provided, item information is calculated for the \eqn{n}th item.
#' @param type A character value which determines the item type:
#' \code{"d"} stands for a dichotomous item, and \code{"p"} stands for a polytomous item.
#'
#' @return
#' A vector of item information values of the same length as \code{x}.
#' @export
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
inform_f_item<- function(x, item.matrix, item, type){
  param <- item.matrix[item,]
  if(type=="d"){
    probs <- P(x, param[1], param[2], param[3])
    probs_ <- first_deriv_dich(x, item.matrix, item)
    inform <- probs_^2/(probs*(1-probs))
  } else if(type=="p"){
    probs <- P_P(x, param[1], param[-1])
    probs_ <- first_deriv_gpcm(x, item.matrix, item)
    inform <- rowSums(probs_^2/probs)
  }
  return(inform)
}

#' Test information function
#'
#' @param x A vector of \eqn{\theta} value(s).
#' @param test An object returned from an estimation function.
#'
#' @return
#' A vector of test information values of the same length as \code{x}.
#' @export
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
inform_f_test <- function(x, test){
  inform <- 0
  if(any(class(test) %in% c("dich", "poly"))){
    param <- test$par_est
    for(i in 1:nrow(param)){
      inform <- inform + inform_f_item(x, param, i, ifelse(any(class(test)=="dich"), "d", "p"))
    }
  } else if(any(class(test) %in% c("mix"))){
    for(j in 1:2){
      param <- test$par_est[[j]]
      for(i in 1:nrow(param)){
        inform <- inform + inform_f_item(x, param, i, ifelse(j==1, "d", "p"))
      }
    }
  }
  return(inform)
}

first_deriv_gpcm <- function(x, item.matrix, item){
  param <- item.matrix[item,]
  cats <- ((1:sum(!is.na(param)))-1)
  probs <- P_P(x, param[1], param[-1])
  ws <- probs%*%cats
  probs_ <- matrix(rep(x = cats, each=length(x)),nrow = length(x))-as.vector(ws)
  probs_ <- probs_*probs*param[1]
  return(probs_)
}

first_deriv_dich <- function(x, item.matrix, item){
  param <- item.matrix[item,]
  probs <- P(x, param[1], param[2], param[3])
  probs_ <- (1-param[3])*(1-probs)*probs*param[1]
  return(probs_)
}
