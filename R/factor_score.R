#' Estimated factor scores
#'
#' @description Factor scores of examinees.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, \code{IRTest_Cont}, or \code{IRTest_Mix}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}), Maximum Likelihood Estimates (\code{MLE}), and weighted likelihood estimates (\code{WLE}).
#' The default is \code{EAP}.
#' @param quad A vector of quadrature points for \code{EAP} calculation.
#' @param prior A vector of the prior distribution for \code{EAP} calculation. The length of it should be the same as \code{quad}.
#'
#' @return
#' \item{theta}{The estimated ability parameter values. If \code{ability_method = "MLE"}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \eqn{\pm}\code{Inf}.}
#' \item{theta_se}{The standard errors of ability parameter estimates.
#' It returns standard deviations of posteriors for \code{EAP}s and asymptotic standard errors (i.e., square root of inverse Fisher information) for \code{MLE}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \code{NA} for \code{MLE}.}
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
    } else if(inherits(x, "cont")){
      E <- Estep_Cont(item=x$par_est, data=x$Options$data, q=length(quad), Xk=quad, Ak=prior)
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
    } else if(inherits(x, "cont")){
      item = x$par_est
      data = x$Options$data
      type = "cont"
    }
    mle_result <- MLE_theta(
      item = item,
      data = data,
      type = type
    )
    theta <- mle_result[[1]]
    theta_se <- mle_result[[2]]
  } else if(ability_method == 'WLE'){
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
    } else if(inherits(x, "cont")){
      item = x$par_est
      data = x$Options$data
      type = "cont"
    }
    wle_result <- WLE_theta(
      item = item,
      data = data,
      type = type
    )
    theta <- wle_result[[1]]
    theta_se <- wle_result[[2]]
  }

  return(list(theta = theta, theta_se = theta_se))
}


#' Ability parameter estimation with fixed item parameters
#'
#' @description
#' Ability parameter estimation when item responses and item parameters are given.
#' This function can be useful in ability parameter estimation is adaptive testing.
#'
#' @param response A matrix of item responses. For mixed-format test, a list
#' of item responses where dichotomous item responses are the first element and
#' polytomous item responses are the second element.
#' @param item A matrix of item parameters. For mixed-format test, a list
#' of item parameters where dichotomous item parameters are the first element and
#' polytomous item parameters are the second element.
#' @param model \code{dich} for dichotomous items,
#' \code{cont} for continuous items, and a specific item response model
#' (e.g., \code{PCM}, \code{GPCM}, \code{GRM}) for polytomous items and a mixed-format test.
#' The default is \code{dich}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}), Maximum Likelihood Estimates (\code{MLE}), and weighted likelihood estimates (\code{WLE}).
#' The default is \code{EAP}.
#' @param quad A vector of quadrature points for \code{EAP} calculation.
#' If \code{NULL} is passed, it is set as \code{seq(-6,6,length.out=121)}. The default is \code{NULL}.
#' @param prior A vector of the prior distribution for \code{EAP} calculation. The length of it should be the same as \code{quad}.
#' If \code{NULL} is passed, the standard normal distribution is used. The default is \code{NULL}.
#'
#' @return
#' \item{theta}{The estimated ability parameter values. If \code{ability_method = "MLE"}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \eqn{\pm}\code{Inf}.}
#' \item{theta_se}{The standard errors of ability parameter estimates.
#' It returns standard deviations of posteriors for \code{EAP}s and asymptotic standard errors (i.e., square root of inverse Fisher information) for \code{MLE}.
#' If an examinee receives a maximum or minimum score for all items, the function returns \code{NA} for \code{MLE}.}
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' # dichotomous
#'
#' response <- c(1,1,0)
#' item <- matrix(
#'   c(
#'       1, -0.5,   0,
#'     1.5,   -1,   0,
#'     1.2,    0, 0.2
#'   ), nrow = 3, byrow = TRUE
#' )
#' adaptive_test(response, item, model = "dich", ability_method = "WLE")
#'
#'
#' # polytomous
#'
#' response <- c(1,2,0)
#' item <- matrix(
#'     c(
#'       1, -0.5, 0.5,
#'     1.5,   -1,   0,
#'     1.2,    0, 0.4
#'     ), nrow = 3, byrow = TRUE
#'   )
#' adaptive_test(response, item, model="GPCM", ability_method = "WLE")
#'
#'
#' # mixed-format test
#'
#' response <- list(c(0,0,0),c(2,2,1))
#' item <- list(
#'   matrix(
#'     c(
#'         1, -0.5, 0,
#'       1.5,   -1, 0,
#'       1.2,    0, 0
#'     ), nrow = 3, byrow = TRUE
#'   ),
#'   matrix(
#'     c(
#'         1, -0.5, 0.5,
#'       1.5,   -1,   0,
#'       1.2,    0, 0.4
#'     ), nrow = 3, byrow = TRUE
#'   )
#' )
#' adaptive_test(response, item, model = "GPCM", ability_method = "WLE")
#'
#'
#' # continuous response
#'
#' response <- c(0.88, 0.68, 0.21)
#' item <- matrix(
#'   c(
#'     1, -0.5, 10,
#'     1.5,   -1,  8,
#'     1.2,    0, 11
#'   ), nrow = 3, byrow = TRUE
#' )
#' adaptive_test(response, item, model = "cont", ability_method = "WLE")
#' }
adaptive_test <- function(response, item, model="dich", ability_method = "EAP", quad=NULL, prior=NULL){
  if((ability_method == "EAP")){
    if((is.null(quad)) & (is.null(prior))){
      quad <- seq(-6,6,length.out=121)
      Ak <- dnorm(quad)
      Ak <- Ak/sum(Ak)
    } else if(is.null(quad)){
      stop("Please specify the locations of quadrature points.")
    } else if(is.null(quad)){
      Ak <- dnorm(quad)
      Ak <- Ak/sum(Ak)
    }
  }

  if(is.list(response) | is.list(item)){  # Mixed-format test
    if(!is.list(response) | !is.list(item)){
      stop("Both 'response' and 'item' should be lists for mixed-format test.")
    }
    if(is.vector(response[[1]])){
      response[[1]] <- matrix(response[[1]], nrow = 1)
    }
    if(is.vector(response[[2]])){
      response[[2]] <- matrix(response[[2]], nrow = 1)
    }
    if(is.vector(item[[1]])){
      item[[1]] <- matrix(item[[1]], nrow = 1)
    }
    if(is.vector(item[[2]])){
      item[[2]] <- matrix(item[[2]], nrow = 1)
    }

    new_list <- structure(
      list(
        par_est = list(Dichotomous=as.matrix(item[[1]]),
                       Polytomous=as.matrix(item[[2]])),
        Options = list(data_D = response[[1]],
                       data_P = response[[2]],
                       model_P = model)
      ),
      class = c("mix", "IRTest", "list")
    )
  } else if(model=="dich"){                 # Dichotomous responses
    if(is.vector(response)){
      response <- matrix(response, nrow = 1)
    }

    if(is.vector(item)){
      item <- matrix(item, nrow = 1)
    }

    new_list <- structure(
      list(
        par_est = as.matrix(item),
        Options = list(data = response)
      ),
      class = c(model, "IRTest", "list")
    )
  } else if(model=="cont"){                   # Continuous responses
    if(is.vector(response)){
      response <- matrix(response, nrow = 1)
    }

    if(is.vector(item)){
      item <- matrix(item, nrow = 1)
    }

    new_list <- structure(
      list(
        par_est = as.matrix(item),
        Options = list(data = response)
      ),
      class = c(model, "IRTest", "list")
    )
  } else{                                     # Polytomous responses
    if(is.vector(response)){
      response <- matrix(response, nrow = 1)
    }

    if(is.vector(item)){
      item <- matrix(item, nrow = 1)
    }

    new_list <- structure(
      list(
        par_est = as.matrix(item),
        Options = list(data = response,
                       model = model)
      ),
      class = c("poly", "IRTest", "list")
    )
  }

  return(factor_score(new_list, ability_method, quad, prior))
}
