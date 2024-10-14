#' Summary of the results
#'
#' @description This function summarizes the output (e.g., convergence of the estimation algorithm, number of parameters, model-fit, and estimated latent distribution).
#'
#' @param object An object of \code{"IRTest"}-class obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param ... Other argument(s).
#'
#' @return Summarized information.
#' @export
#'
#' @examples
#' \donttest{
#' data <- DataGeneration(N=1000, nitem_P = 8)$data_P
#'
#' M1 <- IRTest_Poly(data = data, latent_dist = "KDE")
#'
#' summary(M1)
#'}
summary.IRTest <- function(object, ...){
  sum_result <- list()

  sum_result$convergence <- if(object$diff<=object$Options$threshold){
    stndrdth <- if(object$iter%%10==1){
      "st"
    } else if(object$iter%%10==2){
      "nd"
    } else if(object$iter%%10==3){
      "rd"
    } else {
      "th"
    }
    sprintf("Successfully converged below the threshold of %s on %s%s iterations.",
            as.character(object$Options$threshold),
            as.character(object$iter),
            stndrdth
            )
  } else {
    sprintf("Convergence failed to meet the threshold of %s within %s iterations.",
            as.character(object$Options$threshold),
            as.character(object$Options$max_iter))
  }

  sum_result$n_par <-
    .n_par_used(object)
  sum_result$n_item <-
    .n_item_used(object)
  sum_result$n_respondents <-
    length(object$theta)

  sum_result$model_fit <-
    data.frame(
      ll = -object$logL/2,
      deviance = object$logL,
      AIC = object$logL+2*sum_result$n_par$total,
      BIC = object$logL+log(sum_result$n_respondents)*sum_result$n_par$total,
      HQ = object$logL+2*sum_result$n_par$total*log(log(sum_result$n_respondents))
    )

  sum_result$par_est <- object$par_est
  sum_result$se <- object$se

  sum_result$latent_dist <-
    list(
      method=object$Options$latent_dist,
      x=object$quad,
      y=object$Ak
      )

  return(
    structure(
      sum_result,
      class = c(class(object)[1], 'IRTest_summary', 'list')
    )
  )
}

.n_par_used <- function(object){
  # the number of parameters
  n_par <- data.frame(item = 0, dist = 0, total = 0)

  # item parameters
  if(inherits(object, c("dich"))){
    if(all(object$Options$model %in% c(1, "1PL", "Rasch", "RASCH"))){
      n_par$item <-
        length(object$Options$model)
    }else{
      n_par$item <-
        sum(object$Options$model %in% c(1, "1PL", "Rasch", "RASCH")) +
        2*sum(object$Options$model %in% c(2, "2PL")) +
        3*sum(object$Options$model %in% c(3, "3PL"))
    }
  } else if(inherits(object, c("cont"))){
    if(object$Options$model == 1){
      n_par$item <-
        length(as.vector(object$par_est[,-1]))
    }else{
      n_par$item <-
        length(as.vector(object$par_est))
    }
  } else if(inherits(object, c("poly"))){
    if(object$Options$model == "PCM"){
      n_par$item <- sum(!is.na(object$par_est[,-1]))
    } else if(object$Options$model %in% c("GPCM", "GRM")){
      n_par$item <- sum(!is.na(object$par_est))
    }
  } else if(inherits(object, c("mix"))){
    n_par$item <-
      sum(object$Options$model_D %in% c(1, "1PL", "Rasch", "RASCH")) +
      2*sum(object$Options$model_D %in% c(2, "2PL")) +
      3*sum(object$Options$model_D %in% c(3, "3PL"))
    if(object$Options$model_P == "PCM"){
      n_par$item <- n_par$item +
        sum(!is.na(object$par_est$Polytomous[,-1]))
    } else if(object$Options$model_P %in% c("GPCM", "GRM")){
      n_par$item <- n_par$item +
        sum(!is.na(object$par_est$Polytomous))
    }
  }


  # latent distribution parameters
  # normal distribution
  if(object$Options$latent_dist %in% c("Normal", "normal", "N")){
    n_par$dist <- 0
  }
  # Empirical histogram method
  else if(object$Options$latent_dist=="EHM"){
    n_par$dist <- object$Options$q - 2
  }
  # Two-component normal mixture distribution
  else if(object$Options$latent_dist %in% c("Mixture", "2NM")){
    n_par$dist <- 3
  }
  # Kernel density estimation method
  else if(object$Options$latent_dist=="KDE"){
    n_par$dist <- 1
  }
  # Davidian curve method
  else if(object$Options$latent_dist%in% c("DC", "Davidian")){
    n_par$dist <- object$Options$h
  }
  # Log-linear smoothing
  else if(object$Options$latent_dist%in% c("LLS")){
    n_par$dist <- object$Options$h
  }
  if(inherits(object, c("dich", "poly", "cont"))&all(object$Options$model %in% c(1, "1PL", "Rasch", "RASCH","PCM"))){
    n_par$dist <- n_par$dist+1
  }
  if(inherits(object, c("mix"))&all(c(object$Options$model_D,object$Options$model_P) %in% c(1, "1PL", "Rasch", "RASCH","PCM"))){
    n_par$dist <- n_par$dist+1
  }

  # the total number of parameters
  n_par$total <- n_par$item + n_par$dist

  return(n_par)
}

.n_item_used <- function(object){
  if(inherits(object, c("dich", "poly", "cont"))){
    nrow(object$par_est)
  } else if(inherits(object, c("mix"))){
    data.frame(dich = nrow(object$par_est$Dichotomous),
               poly = nrow(object$par_est$Polytomous))
  }
}

