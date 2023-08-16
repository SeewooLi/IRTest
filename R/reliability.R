#' Marginal reliability coefficient of IRT
#'
#' @param x
#'
#' @return Estimated marginal reliability coefficient
#' @export
#'
#' @examples
reliability <- function(x){
  Ak <- x$Ak
  Xk <- x$quad
  param <- x$par_est

  if(any(class(x)%in%c("dich", "poly"))){
    n_item <- nrow(param)
    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item)
    squared <- matrix(nrow = length(Xk), ncol = n_item)

    if(any(class(x)=="dich")){
      for(i in 1:n_item){
        true_score_matrix[,i] <- P(Xk, a = param[i,1], b = param[i,2], c = param[i,3])
        squared[,i] <- true_score_matrix[,i]
      }
    } else if(any(class(x)=="poly")){
      cats <- x$Options$categories
      for(i in 1:n_item){
        n_cat <- sum(!is.na(param[i,]))
        true_score_matrix[,i] <- P_P(Xk, a = param[i,1], b = param[i,-1])%*%cats[[i]]
        squared[,i] <- P_P(Xk, param[i,1], param[i,-1])%*%cats[[i]]^2
      }
    }
  } else if(any(class(x)=="mix")){
    n_item_D <- nrow(param[[1]])
    n_item_P <- nrow(param[[2]])

    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)
    squared <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)

    cats <- x$Options$categories
    for(i in 1:n_item_D){
      true_score_matrix[,i] <- P(Xk, a = param[i,1], b = param[i,2], c = param[i,3])
      squared[,i] <- true_score_matrix[,i]
    }
    for(i in n_item_D+(1:n_item_P)){
      n_cat <- sum(!is.na(param[i,]))
      true_score_matrix[,i] <- P_P(Xk, a = param[i,1], b = param[i,-1])%*%cats[[i]]
      squared[,i] <- P_P(Xk, param[i,1], param[i,-1])%*%cats[[i]]^2
    }
  }

  sigma2_e <- sum(Ak%*%(squared-true_score_matrix^2))
  mu_T <- rowSums(true_score_matrix)%*%Ak
  sigma2_T <- (rowSums(true_score_matrix)^2)%*%Ak-mu_T^2
  return(
    as.vector(sigma2_T/(sigma2_T+sigma2_e))
    )
}
