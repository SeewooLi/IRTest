#' Estimated factor scores
#'
#' @description Factor scores of examinees.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}) and Maximum Likelihood Estimates (\code{MLE}).
#' The default is \code{EAP}.
#' @param quad A vector of quadrature points for \code{EAP} calculation.
#' @param prior A vector of the prior distribution for \code{EAP} calculation. The length of it should be the same as \code{quad}.
#'
#' @return
#' \item{theta}{The estimated ability parameter values. If \code{ability_method = "MLE"}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \eqn{\pm}\code{Inf}.}
#' \item{theta_se}{The asymptotic standard errors of ability parameter estimates. If an examinee receives a maximum or minimum score for all items, the function returns \code{NA}.}
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
#' # Item fit statistics
#'
#' factor_score(M1, ability_method = "MLE")
#'}
factor_score <- function(x, ability_method = "EAP", quad=NULL, prior=NULL){
  if(ability_method == 'EAP'){
    if(is.null(quad)){
      quad <- x$quad
    }
    if(is.null(prior)){
      prior <- x$Ak
    }
    if(inherits(x, "dich")){
      E <- Estep(item=x$par_est, data=x$Options$data, q=length(quad), Xk=quad, Ak=prior)
    } else if(inherits(x, "poly")){
      E <- Estep_Poly(item=x$par_est, data=x$Options$data, q=length(quad), Xk=quad, Ak=prior, model=x$Options$model)
    } else if(inherits(x, "mix")){
      E <- Estep_Mix(item_D=x$par_est$Dichotomous, item_P=x$par_est$Polytomous,
                     data_D=x$Options$data_D, data_P=x$Options$data_P,
                     q=length(quad), Xk=quad, Ak=prior, model=x$Options$model_P)
    }
    theta <- as.numeric(E$Pk%*%E$Xk)
    theta_se <- sqrt(as.numeric(E$Pk%*%(E$Xk^2))-theta^2)
    # theta <- as.numeric(x$Pk%*%x$quad)
    # theta_se <- NULL
  } else if(ability_method == 'MLE'){
    if(inherits(x, "dich")){
      item = x$par_est
      data = x$Options$data
      type = "dich"
    } else if(inherits(x, "poly")){
      item = x$par_est
      data = x$Options$data
      type = x$Options$model
    } else if(inherits(x, "mix")){
      item = list(x$par_est$Dichotomous,x$par_est$Polytomous)
      data = list(x$Options$data_D, x$Options$data_P)
      type = c("mix", x$Options$model_P)
    }
    mle_result <- MLE_theta(
      item = item,
      data = data,
      type = type
    )
    theta <- mle_result[[1]]
    theta_se <- mle_result[[2]]
  }

  return(list(theta = theta, theta_se = theta_se))
}
