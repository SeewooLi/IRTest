#' Estimated factor scores
#'
#' @description Factor scores of examinees.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}) and Maximum Likelihood Estimates (\code{MLE}).
#' The default is \code{EAP}.
#'
#' @return
#' \item{theta}{The estimated ability parameter values. If \code{ability_method = "MLE"}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \eqn{\pm}\code{Inf}.}
#' \item{theta_se}{The asymptotic standard errors of ability parameter estimates. Available only when \code{ability_method = "MLE"}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \code{NA}.}
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @export
#' @examples
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
#'
factor_score <- function(x, ability_method = "EAP"){
  if(ability_method == 'EAP'){
    theta <- as.numeric(x$Pk%*%x$quad)
    theta_se <- NULL
  } else if(ability_method == 'MLE'){
    type <- if(inherits(x, "dich")){
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
