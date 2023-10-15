#' Marginal reliability coefficient of IRT
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#'
#' @return Estimated marginal reliability coefficients.
#'
#' @importFrom stats integrate
#'
#' @details
#' \describe{
#' \item{Reliability coefficient on summed-score scale}{
#' In accordance with the concept of \emph{reliability} in classical test theory (CTT),
#' this function calculates the IRT reliability coefficients.
#'
#' The basic concept and formula of the reliability coefficient can be expressed as follows (Kim & Feldt, 2010):
#'
#' An observed score of Item \eqn{i}, \eqn{X_i}, is decomposed as the sum of a true score \eqn{T_i} and an error \eqn{e_i}.
#' Then, with the assumption of \eqn{\sigma_{T_{i}e_{j}}=\sigma_{e_{i}e_{j}}=0}, the reliability coefficient of a test is defined as;
#' \deqn{\rho_{TX}=\rho_{XX^{'}}=\frac{\sigma_{T}^{2}}{\sigma_{X}^{2}}=\frac{\sigma_{T}^{2}}{\sigma_{T}^{2}+\sigma_{e}^{2}}=1-\frac{\sigma_{e}^{2}}{\sigma_{X}^{2}}}
#' }
#'
#' \item{Reliability coefficient on \eqn{\theta} scale}{
#' For the coefficient on the \eqn{\theta} scale, this function calculates the parallel-forms reliability (Green et al., 1984; Kim, 2012):
#' \deqn{
#' \rho_{\hat{\theta} \hat{\theta}^{'}}
#' =\frac{\sigma_{E\left(\hat{\theta}\mid \theta \right )}^{2}}{\sigma_{E\left(\hat{\theta}\mid \theta \right )}^{2}+E\left( \sigma_{\hat{\theta}|\theta}^{2} \right)}
#' =\frac{1}{1+E\left(I\left(\hat{\theta}\right)^{-1}\right)}
#' }
#' This assumes that \eqn{\sigma_{E\left(\hat{\theta}\mid \theta \right )}^{2}=\sigma_{\theta}^{2}=1}.
#' Although the formula is often employed in several IRT studies and applications, the underlying assumption may not be true.
#' }
#' }
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Green, B.F., Bock, R.D., Humphreys, L.G., Linn, R.L., & Reckase, M.D. (1984). Technical guidelines for assessing computerized adaptive tests. \emph{Journal of Educational Measurement, 21}(4), 347–360.
#'
#' Kim, S. (2012). A note on the reliability coefficients for item response model-based ability estimates. \emph{Psychometrika, 77}(1), 153-162.
#'
#' Kim, S., Feldt, L.S. (2010). The estimation of the IRT reliability coefficient and its lower and upper bounds, with comparisons to CTT reliability statistics. \emph{Asia Pacific Education Review, 11}, 179–188.
#'
#'
#'
#' @export
#'
#' @examples
#' data <- DataGeneration(N=500, nitem_D = 10)$data_D
#'
#' # Analysis
#'
#' M1 <- IRTest_Dich(data)
#'
#'
#' # Reliability coefficients
#' reliability(M1)
#'
reliability <- function(x){

  # Summed score scale
  Ak <- x$Ak
  Xk <- x$quad
  param <- x$par_est

  if(inherits(x, c("dich", "poly"))){
    n_item <- nrow(param)
    true_score_matrix <- matrix(nrow = length(Xk), ncol = n_item)
    squared <- matrix(nrow = length(Xk), ncol = n_item)

    cats <- x$Options$categories

    if(inherits(x, c("dich"))){
      for(i in 1:n_item){
        ppp <- P(Xk, a = param[i,1], b = param[i,2], c = param[i,3])
        true_score_matrix[,i] <- cbind(1-ppp,ppp)%*%cats[[i]]
        squared[,i] <- cbind(1-ppp,ppp)%*%cats[[i]]^2
      }
    } else if(inherits(x, c("poly"))){
      for(i in 1:n_item){
        if(x$Options$model %in% c("PCM", "GPCM")){
          ppp <- P_P(Xk, a = param[i,1], b = param[i,-1])
        } else if(x$Options$model %in% c("GRM")){
          ppp <- P_G(Xk, a = param[i,1], b = param[i,-1])
        }
        true_score_matrix[,i] <- ppp%*%cats[[i]]
        squared[,i] <- ppp%*%cats[[i]]^2
      }
    }
  } else if(inherits(x, c("mix"))){
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
      if(x$Options$model_P %in% c("PCM", "GPCM")){
        ppp <- P_P(Xk, a = param$Polytomous[i,1], b = param$Polytomous[i,-1])
      } else if(x$Options$model_P %in% c("GRM")){
        ppp <- P_G(Xk, a = param$Polytomous[i,1], b = param$Polytomous[i,-1])
      }
      true_score_matrix[,n_item_D+i] <- ppp%*%cats$Polytomous[[i]]
      squared[,n_item_D+i] <- ppp%*%cats$Polytomous[[i]]^2
    }
  }

    ## test level
    sigma2_e <- sum(Ak%*%(squared-true_score_matrix^2))
    mu_T <- rowSums(true_score_matrix)%*%Ak
    sigma2_T <- (rowSums(true_score_matrix)^2)%*%Ak-mu_T^2
    rxx1 <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    names(rxx1) <- "test reliability"

    ## item level
    sigma2_e <- Ak%*%(squared-true_score_matrix^2)
    mu_T <- Ak%*%true_score_matrix
    sigma2_T <- Ak%*%true_score_matrix^2-mu_T^2
    rxx2 <- as.vector(sigma2_T/(sigma2_T+sigma2_e))
    if(inherits(x, c("mix"))){
      names(rxx2) <- c(paste(row.names(param$Dichotomous),"D", sep = "_"),
                       paste(row.names(param$Polytomous),"P", sep = "_"))
    } else {
      names(rxx2) <- row.names(param)
    }

  # theta scale
    if(x$Options$latent_dist %in% c("EHM", "LLS")){
      inform <- as.vector((1/inform_f_test(x$quad, x))%*%x$Ak)
    } else {
      inform <- integrate(f=function(theta)1/inform_f_test(theta, x)*latent_distribution(theta, x),
                          lower = -6, upper = 6)$value
    }
    rxx3 <- 1/(inform+1)
    names(rxx3) <- "test reliability"

    return(
      list(
        summed.score.scale = list(
          test = rxx1,
          item = rxx2),
        theta.scale = rxx3
      )
    )
}
