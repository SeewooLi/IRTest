#' Marginal reliability coefficient of IRT
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#'
#' @return Estimated marginal reliability coefficient.
#'
#' @details
#' \describe{
#' In accordance with the concept of \emph{reliability} in classical test theory (CTT),
#' this function calculates the IRT reliability coefficient.
#' \item{
#' The basic concept and formula of the reliability coefficient can be expressed as follows (Kim, Feldt, 2010):
#' }{
#' An observed score of Item \eqn{i} (\eqn{X_i}) is decomposed as the sum of a true score \eqn{T_i} and an error \eqn{e_i}.
#' Then, with the assumption of \eqn{\sigma_{T_{i}e_{j}}=\sigma_{e_{i}e_{j}}=0}, the reliability coefficient of a test is defined as;
#' \deqn{\rho_{TX}=\rho_{XX^{'}}=\frac{\sigma_{T}^{2}}{\sigma_{X}^{2}}=\frac{\sigma_{T}^{2}}{\sigma_{T}^{2}+\sigma_{e}^{2}}=1-\frac{\sigma_{e}^{2}}{\sigma_{X}^{2}}}
#' }
#' }
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Kim, S., Feldt, L.S. (2010). The estimation of the IRT reliability coefficient and its lower and upper bounds, with comparisons to CTT reliability statistics. \emph{Asia Pacific Education Review, 11}, 179–188.
#'
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
reliability <- function(x){
  Ak <- x$Ak
  Xk <- x$quad
  param <- x$par_est

  if(any(class(x)%in%c("dich", "poly"))){
    n_item <- nrow(param)
    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item)
    squared <- matrix(nrow = length(Xk), ncol = n_item)

    cats <- x$Options$categories

    if(any(class(x)=="dich")){
      for(i in 1:n_item){
        ppp <- P(Xk, a = param[i,1], b = param[i,2], c = param[i,3])
        true_score_matrix[,i] <- cbind(1-ppp,ppp)%*%cats[[i]]
        squared[,i] <- cbind(1-ppp,ppp)%*%cats[[i]]^2
      }
    } else if(any(class(x)=="poly")){
      for(i in 1:n_item){
        ppp <- P_P(Xk, a = param[i,1], b = param[i,-1])
        true_score_matrix[,i] <- ppp%*%cats[[i]]
        squared[,i] <- ppp%*%cats[[i]]^2
      }
    }
  } else if(any(class(x)=="mix")){
    n_item_D <- nrow(param$Dichotomous)
    n_item_P <- nrow(param$Polytomous)

    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)
    squared <- matrix(nrow = length(Xk), ncol = n_item_D+n_item_P)

    cats <- x$Options$categories

    for(i in 1:n_item_D){
      ppp <- P(Xk,
               a = param$Dichotomous[i,1],
               b = param$Dichotomous[i,2],
               c = param$Dichotomous[i,3])
      true_score_matrix[,i] <- cbind(1-ppp,ppp)%*%cats$Dichotomous[[i]]
      squared[,i] <- cbind(1-ppp,ppp)%*%cats$Dichotomous[[i]]^2
    }
    for(i in 1:n_item_P){
      ppp <- P_P(Xk, a = param$Polytomous[i,1], b = param$Polytomous[i,-1])
      true_score_matrix[,n_item_D+i] <- ppp%*%cats$Polytomous[[i]]
      squared[,n_item_D+i] <- ppp%*%cats$Polytomous[[i]]^2
    }
  }

  # test level
    sigma2_e <- sum(Ak%*%(squared-true_score_matrix^2))
    mu_T <- rowSums(true_score_matrix)%*%Ak
    sigma2_T <- (rowSums(true_score_matrix)^2)%*%Ak-mu_T^2
    rxx1 <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    names(rxx1) <- "test reliability"

  # item level
    sigma2_e <- Ak%*%(squared-true_score_matrix^2)
    mu_T <- Ak%*%true_score_matrix
    sigma2_T <- Ak%*%true_score_matrix^2-mu_T^2
    rxx2 <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    names(rxx2) <- 1:length(rxx2)

    return(
      list(
        rel.summed.score.test = rxx1,
        rel.summed.score.item = rxx2
      )
    )
}
