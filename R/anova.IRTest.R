#' Model comparison
#'
#' @param ... Objects of \code{"IRTest"}-class to be compared.
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @return Model-fit indices and results of likelihood ratio test (LRT).
#' @export
#'
anova.IRTest <- function(...){
  obs <- list(...)
  if(length(obs) < 2){
    stop("More than two models from a same data should be provided.")
  }
  logLik <- NULL
  deviance <- NULL
  AIC <- NULL
  BIC <- NULL
  HQ <- NULL
  n_pars <- NULL
  chi <- NULL
  p_value <- NULL
  if(is.null(names(match.call()))){
    model_names <- unlist(lapply(substitute(list(...)), deparse)[-1])
  } else {
    model_names <- names(match.call())
  }


  for(i in 1:length(obs)){
    sumobs <- summary(obs[[i]])

    logLik <- append(logLik, sumobs$model_fit$ll)
    deviance <- append(deviance, sumobs$model_fit$deviance)
    AIC <- append(AIC, sumobs$model_fit$AIC)
    BIC <- append(BIC, sumobs$model_fit$BIC)
    HQ <- append(HQ, sumobs$model_fit$HQ)
    n_pars <- append(n_pars, sumobs$n_par$total)
    chi <- append(chi, ifelse(i==1, NA, -(deviance[i-1]-deviance[i])/(n_pars[i-1]-n_pars[i])))
    p_value <- append(p_value, ifelse(i==1, NA, pchisq(chi[i], df = abs(n_pars[i-1]-n_pars[i]), lower.tail = FALSE)))
  }

  return(
    structure(
      data.frame(
        logLik = logLik,
        deviance = deviance,
        AIC = AIC,
        BIC = BIC,
        HQ = HQ,
        n_pars = n_pars,
        chi = chi,
        p_value = p_value,
        row.names = if(length(obs)==length(model_names)) model_names else model_names[-1]
      ),
      class = c('anova_IRTest', 'data.frame')
    )
  )
}

#' @export
print.anova_IRTest <- function(x, ...){
  cat("Result of model comparison\n\n")
  x$p_value <- round(x$p_value, 4)
  print(format(x), quote=FALSE)
  invisible(x)
}

#' Selecting the best model
#'
#' @param ... Candidate models
#' @param criterion The criterion to be used. The default is \code{HQ}.
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @return The best model and model-fit indices.
#' @export
#'
best_model <- function(..., criterion = "HQ"){
  mcres <- anova.IRTest(...)
  nms <- row.names(mcres)
  mcres <- mcres[[criterion]]
  res <- data.frame(
    mcres,
    row.names = nms
  )
  colnames(res) <- criterion
  structure(
    list(
      best = if(criterion == "logLik"){
        nms[which.max(mcres)]
        } else nms[which.min(mcres)],
      criterion = res
    ),
    class = c("best_model", "list")
  )
}

#' @export
print.best_model <- function(x, ...){
  cat("The best model:", x$best,"\n\n")

  print(format(x$criterion), quote=FALSE)
  invisible(x)
}
