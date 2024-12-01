#' Item information function
#'
#' @param x A vector of \eqn{\theta} value(s).
#' @param test An object returned from an estimation function.
#' @param item A natural number indicating the \eqn{n}th item.
#' @param type A character value for a mixed format test which determines the item type:
#' \code{"d"} and \code{"p"} stand for a dichotomous and polytomous item, respectively.
#'
#' @return
#' A vector of the evaluated item information values.
#' @export
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
inform_f_item<- function(x, test, item = 1, type = "d"){
  if(inherits(test, "dich")){
    param <- test$par_est[item,]
    probs <- P(x, param[1], param[2], param[3])
    probs_ <- first_deriv_dich(x, param)
    inform <- probs_^2/(probs*(1-probs))
  } else if(inherits(test, "cont")){
    param <- test$par_est[item,]
    inform <- L2_Cont(theta = x, a = param[1], b = param[2], nu = param[3])
  } else if(inherits(test, "poly")){
    param <- test$par_est[item,]
    param <- param[!is.na(param)]
    if(test$Options$model %in% c("PCM", "GPCM")){
      probs <- P_P(x, param[1], param[-1])
      probs_ <- first_deriv_gpcm(x, param)
    } else if(test$Options$model %in% c("GRM")){
      probs <- P_G(x, param[1], param[-1])
      probs_ <- first_deriv_grm(x, param)
    }else if(test$Options$model %in% c("likert")){
      if(length(test$Option$ncats)==1){
        ncats <- test$Option$ncats
      } else {
        ncats <- test$Option$ncats[item.number]
      }
      probs <- likert(x, a = param[1], b = param[2], nu = param[3], ncats = ncats)
      probs_ <- first_deriv_likert(x, param, ncats)
    }
    if(is.null(nrow(probs))){
      inform <- sum((probs_^2)/probs)
    } else {
      inform <- rowSums((probs_^2)/probs)
    }

  } else if(inherits(test, "mix")){
    if(type == "d"){
      param <- test$par_est[[1]][item,]
      probs <- P(x, param[1], param[2], param[3])
      probs_ <- first_deriv_dich(x, param)
      inform <- probs_^2/(probs*(1-probs))
    } else if(type == "p"){
      param <- test$par_est[[2]][item,]
      param <- param[!is.na(param)]
      if(test$Options$model_P %in% c("PCM", "GPCM")){
        probs <- P_P(x, param[1], param[-1])
        probs_ <- first_deriv_gpcm(x, param)
      } else if(test$Options$model_P %in% c("GRM")){
        probs <- P_G(x, param[1], param[-1])
        probs_ <- first_deriv_grm(x, param)
      }
      if(is.null(nrow(probs))){
        inform <- sum((probs_^2)/probs)
      } else {
        inform <- rowSums((probs_^2)/probs)
      }
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
  if(inherits(test, c("dich", "poly", "cont"))){
    for(i in 1:nrow(test$par_est)){
      inform <- inform + inform_f_item(x, test, i)
    }
  } else if(inherits(test, "mix")){
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
  param <- param[!is.na(param)]
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

first_deriv_likert <- function(x, param, ncats){
  grids <- seq(0.005,0.995, length=100)
  cut_score <- (1:(ncats-1))/ncats
  ind_cat <- as.numeric(cut(grids,breaks = c(0,cut_score,1),labels = 1:ncats))

  mu <- P(X, param[1], param[2])
  nu <- param[3]
  p0 <- likert(X, a = param[1], b = param[2], nu = nu, ncats = ncats)

  pmat <- t(outer(grids, nu*mu-1, FUN = "^")*outer(1-grids, nu*(1-mu)-1, FUN = "^"))/beta(nu*mu,nu*(1-mu)) # probability matrix wo the normalizing factor
  l1th <- nu*pmat*t(outer(log(grids/(1-grids)), digamma(nu*mu)-digamma(nu*(1-mu)), FUN = "-"))
  l1t <- matrix(ncol = ncats, nrow = length(X))
  for(c in 1:ncats){
    l1t[,c] <- rowSums(l1th[,ind_cat==c])*0.01
  }
  l1t <- param[1]*mu*(1-mu)*l1t
  return(l1t)
}
