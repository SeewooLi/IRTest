#' Item information function
#'
#' @param x A vector of \eqn{\theta} value(s).
#' @param test An object returned from an estimation function.
#' @param item A numeric value indicating an item.
#' If \eqn{n} is provided, item information is calculated for the \eqn{n}th item.
#' @param type A character value for a mixed format test which determines the item type:
#' \code{"d"} stands for a dichotomous item, and \code{"p"} stands for a polytomous item.
#'
#' @return
#' A vector of item information values of the same length as \code{x}.
#' @export
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
inform_f_item<- function(x, test, item, type = NULL){
  if(any(class(test) == "dich")){
    param <- test$par_est[item,]
    probs <- P(x, param[1], param[2], param[3])
    probs_ <- first_deriv_dich(x, param)
    inform <- probs_^2/(probs*(1-probs))
  } else if(any(class(test) == "poly")){
    param <- test$par_est[item,]
    if(test$Options$model %in% c("PCM", "GPCM")){
      probs <- P_P(x, param[1], param[-1])
      probs_ <- first_deriv_gpcm(x, param)
    } else if(test$Options$model %in% c("GRM")){
      probs <- P_G(x, param[1], param[-1])
      probs_ <- first_deriv_grm(x, param)
    }
    inform <- rowSums((probs_^2)/probs)
  } else if(any(class(test) == "mix")){
    if(type == "d"){
      param <- test$par_est[[1]][item,]
      probs <- P(x, param[1], param[2], param[3])
      probs_ <- first_deriv_dich(x, param)
      inform <- probs_^2/(probs*(1-probs))
    } else if(type == "p"){
      param <- test$par_est[[2]][item,]
      if(test$Options$model_P %in% c("PCM", "GPCM")){
        probs <- P_P(x, param[1], param[-1])
        probs_ <- first_deriv_gpcm(x, param)
      } else if(test$Options$model_P %in% c("GRM")){
        probs <- P_G(x, param[1], param[-1])
        probs_ <- first_deriv_grm(x, param)
      }
      inform <- rowSums((probs_^2)/probs)
    }
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
    for(i in 1:nrow(test$par_est)){
      inform <- inform + inform_f_item(x, test, i)
    }
  } else if(any(class(test) %in% c("mix"))){
    for(j in 1:2){
      for(i in 1:nrow(test$par_est[[j]])){
        inform <- inform + inform_f_item(x, test, i, c("d", "p")[j])
      }
    }
  }
  return(inform)
}

first_deriv_grm <- function(x, param){
  param <- param[!is.na(param)]
  probs_ <- outer(X = x, Y = param[-1], FUN = P2, a=param[1])
  probs_ <- param[1]*cbind(0, probs_*(1-probs_), 0)
  probs_ <- probs_[,-ncol(probs_)]-probs_[,-1]
  return(probs_)
}

first_deriv_gpcm <- function(x, param){
  cats <- ((1:sum(!is.na(param)))-1)
  probs <- P_P(x, param[1], param[-1])
  ws <- as.vector(probs%*%cats)
  probs_ <- matrix(rep(x = cats, each=length(x)),nrow = length(x))-ws
  probs_ <- probs_*probs*param[1]
  return(probs_)
}

first_deriv_dich <- function(x, param){
  probs <- P(x, param[1], param[2], param[3])
  probs_ <- (1-param[3])*(1-probs)*probs*param[1]
  return(probs_)
}
