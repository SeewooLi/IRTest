#' Marginal reliability coefficient of IRT
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#' @param level A character value of either \code{"test"} or \code{"item"}
#' which is the level of reliability coefficient(s) to be returned.
#' The default is \code{"test"}.
#' @return Estimated marginal reliability coefficient
#' @export
#'
#' @examples
#' \dontrun{
#' Alldata <- DataGeneration(seed = 1,
#'                           model_D = rep(1, 10),
#'                           N=500,
#'                           nitem_D = 10,
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_D
#'
#' # Analysis
#'
#' M1 <- IRTest_Dich(data)
#'
#' reliability(M1)
#'}
reliability <- function(x, level = "test"){
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
      for(i in 1:n_item){
        n_cat <- sum(!is.na(param[i,]))
        true_score_matrix[,i] <- P_P(Xk, a = param[i,1], b = param[i,-1])%*%(0:(n_cat-1))
        squared[,i] <- P_P(Xk, param[i,1], param[i,-1])%*%(0:(n_cat-1))^2
      }
    }
  } else if(any(class(x)=="mix")){
    n_item_D <- nrow(param[[1]])
    n_item_P <- nrow(param[[2]])

    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)
    squared <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)

    for(i in 1:n_item_D){
      true_score_matrix[,i] <- P(Xk, a = param[i,1], b = param[i,2], c = param[i,3])
      squared[,i] <- true_score_matrix[,i]
    }
    for(i in n_item_D+(1:n_item_P)){
      n_cat <- sum(!is.na(param[i,]))
      true_score_matrix[,i] <- P_P(Xk, a = param[i,1], b = param[i,-1])%*%(0:(n_cat-1))
      squared[,i] <- P_P(Xk, param[i,1], param[i,-1])%*%(0:(n_cat-1))^2
    }
  }

  if(level=="test"){
    sigma2_e <- sum(Ak%*%(squared-true_score_matrix^2))
    mu_T <- rowSums(true_score_matrix)%*%Ak
    sigma2_T <- (rowSums(true_score_matrix)^2)%*%Ak-mu_T^2
    rxx <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    names(rxx) <- "test reliability"
    return(
      rxx
    )
  } else if(level=="item"){
    sigma2_e <- Ak%*%(squared-true_score_matrix^2)
    mu_T <- Ak%*%true_score_matrix
    sigma2_T <- Ak%*%true_score_matrix^2-mu_T^2
    rxx <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    names(rxx) <- 1:length(rxx)
    return(
      rxx
    )
  }
}
