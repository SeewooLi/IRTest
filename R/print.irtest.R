#' Print of the result
#'
#' @description This function prints the summarized information.
#'
#' @param x An object of \code{"IRTest"}-class obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, or \code{\link{IRTest_Mix}}.
#' @param ... Additional arguments (currently non-functioning).
#'
#' @return Printed texts on the console recommending the usage of \code{summary} function and the direct access to the details using "$" sign.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' data <- DataGeneration(N=1000,
#'                        nitem_P = 8,
#'                        latent_dist = "2NM",
#'                        d = 1.664,
#'                        sd_ratio = 2,
#'                        prob = 0.3)$data_P
#'
#' M1 <- IRTest_Poly(data = data, latent_dist = "KDE")
#'
#' M1
#'
print.IRTest <- function(x, ...){
  cat('Convergence: ', '\n')
  if(x$diff<=x$Options$threshold){
    cat("Successfully converged.",'\n')
  } else {
    cat("Convergence failed.", '\n')
  }
  cat('\n')
  cat('The Latent Distribution Estimation Method: ', '\n')
  cat(x$Options$latent_dist, '\n')
  cat('\n')
  cat('Class: \n', class(x)[1], '\n')
  cat('\n')
  cat('<NOTE> \n')
  cat('For more information; \n')
  cat('Use \n')
  cat('1) `$` sign for the direct access to the elements \n')
  cat('and/or \n')
  cat('2) `summary` function for the briefly summarized output. \n')
  invisible(x)
}

#' Print of the summary
#'
#' @description This function prints the summarized information.
#'
#' @param x An object returned from \code{\link{summary.IRTest}}.
#' @param ... Additional arguments (currently non-functioning).
#' @return Printed summarized texts on the console.
#' @export
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @examples
#' data <- DataGeneration(N=1000,
#'                           nitem_P = 8,
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)$data_P
#'
#' M1 <- IRTest_Poly(data = data,
#'                   latent_dist = "Mixture",
#'                   )
#'
#' summary(M1)
#'
print.IRTest_summary <- function(x, ...){
  .prt.irtest.convergence(x)
  .prt.irtest.model_fit(x)
  .prt.irtest.n_par(x)
  .prt.irtest.n_item(x)
  .prt.irtest.latent(x)
  invisible(x)
}


.prt.irtest.convergence <- function(object){
  cat('Convergence: ', '\n')
  cat(object$convergence, '\n')
  cat('\n')
}

.prt.irtest.model_fit <- function(object){
  cat('Model Fit: ', '\n')
  cat(' log-likeli  ', -object$model_fit$deviance/2,'\n')
  cat('   deviance  ', object$model_fit$deviance,'\n')
  cat('        AIC  ', object$model_fit$AIC,'\n')
  cat('        BIC  ', object$model_fit$BIC,'\n')
  cat('         HQ  ', object$model_fit$HQ,'\n')
  cat('\n')
}

.prt.irtest.n_par <- function(object){
  cat('The Number of Parameters: ', '\n')
  cat('       item  ', object$n_par$item, '\n')
  cat('       dist  ', object$n_par$dist, '\n')
  cat('      total  ', object$n_par$total, '\n')
  cat('\n')
}

.prt.irtest.n_item <- function(object){
  cat('The Number of Items: ', '\n')
  cat('dichotomous  ', object$n_item$dich, '\n')
  cat('polyotomous  ', object$n_item$poly, '\n')
  cat('\n')
}

.prt.irtest.latent <- function(object){
  simple_ld <-
    approx(
      x = object$latent_dist$x,
      y = object$latent_dist$y,
      xout = seq(-2,2,length=21),
      method = "linear",
      rule=2
    )$y
  simple_ld <- simple_ld/sum(simple_ld)
  cat('The Estimated Latent Distribution: ', '\n')
  cat('method -', object$latent_dist$method, '\n')
  cat(rep('-',40),'\n',sep='')
  tf <- rep(FALSE, 21)
  for(i in 10:1){
    cat(paste(ifelse(tf|((simple_ld-i/100)>0.005),'@', ifelse((simple_ld-i/100)>0,'.', ' ')), sep = ''),'\n')
    tf <- tf|(simple_ld-i/100)>0
  }
  cat('+',rep('-',9),'+',rep('-',9),'+',rep('-',9),'+',rep('-',9),'+','\n',sep='')
  cat('-2',rep(' ',8),'-1',rep(' ',8),'0',rep(' ',9),'1',rep(' ',9),'2','\n',sep='')
  cat('\n')
}

