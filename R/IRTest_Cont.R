#' Item and ability parameters estimation for continuous response items
#'
#' @description This function estimates IRT item and ability parameters when all items are scored dichotomously.
#' Based on Bock & Aitkin's (1981) marginal maximum likelihood and EM algorithm (EM-MML), this function provides several latent distribution estimation algorithms which could free the normality assumption on the latent variable.
#' If the normality assumption is violated, application of these latent distribution estimation methods could reflect non-normal characteristics of the unknown true latent distribution,
#' and, thus, could provide more accurate parameter estimates (Li, 2021; Woods & Lin, 2009; Woods & Thissen, 2006).
#'
#' @importFrom stats density nlminb
#' @importFrom utils flush.console
#'
#' @param data A matrix or data frame of item responses where responses are coded as 0 or 1.
#' Rows and columns indicate examinees and items, respectively.
#' @param range Range of the latent variable to be considered in the quadrature scheme.
#' The default is from \code{-6} to \code{6}: \code{c(-6, 6)}.
#' @param q A numeric value that represents the number of quadrature points. The default value is 121.
#' @param initialitem A matrix of initial item parameter values for starting the estimation algorithm. The default value is \code{NULL}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}) and Maximum Likelihood Estimates (\code{MLE}).
#' The default is \code{EAP}.
#' @param latent_dist A character string that determines latent distribution estimation method.
#' Insert \code{"Normal"}, \code{"normal"}, or \code{"N"} for the normality assumption on the latent distribution,
#' \code{"EHM"} for empirical histogram method (Mislevy, 1984; Mislevy & Bock, 1985),
#' \code{"2NM"} or \code{"Mixture"} for using two-component Gaussian mixture distribution (Li, 2021; Mislevy, 1984),
#' \code{"DC"} or \code{"Davidian"} for Davidian-curve method (Woods & Lin, 2009),
#' \code{"KDE"} for kernel density estimation method (Li, 2022),
#' and \code{"LLS"} for log-linear smoothing method (Casabianca & Lewis, 2015).
#' The default value is set to \code{"Normal"} to follow the convention.
#' @param max_iter A numeric value that determines the maximum number of iterations in the EM-MML.
#' The default value is 200.
#' @param threshold A numeric value that determines the threshold of EM-MML convergence.
#' A maximum item parameter change is monitored and compared with the threshold.
#' The default value is 0.0001.
#' @param bandwidth A character value that can be used if \code{latent_dist = "KDE"}.
#' This argument determines the bandwidth estimation method for \code{"KDE"}.
#' The default value is \code{"SJ-ste"}. See \code{\link{density}} for available options.
#' @param h A natural number less than or equal to 10 if \code{latent_dist = "DC" or "LLS"}.
#' This argument determines the complexity of the distribution.
#'
#' @return
#' @export
#'
#' @examples
IRTest_Cont <- function(data, range = c(-6,6), q = 121, initialitem=NULL,
                        ability_method = 'EAP', latent_dist="Normal", max_iter=200,
                        threshold=0.0001, bandwidth="SJ-ste", h=NULL){

  # categories <- apply(data, MARGIN = 2, FUN = extract_cat, simplify = FALSE)
  #
  # if(
  #   !all(
  #     unlist(
  #       lapply(categories, length)
  #     )==2
  #   )
  # ) stop("Not all items are dichotomously scored.")
  #
  # data <- reorder_mat(as.matrix(data))

  if(is.null(initialitem)){
    initialitem <- matrix(rep(c(1,0,10), each = ncol(data)), ncol = 3)
  }

  Options = list(initialitem=initialitem, data=data, range=range, q=q,
                 ability_method=ability_method,latent_dist=latent_dist,
                 max_iter=max_iter, threshold=threshold,bandwidth=bandwidth, h=h#,categories=categories
                 )

  I <- initialitem
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data)
  density_par <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item = initialitem, data = data, range = range, q = q, Xk=Xk, Ak=Ak)
      initialitem <- Mstep_Cont(E, initialitem, data)

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
        )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    Ak <- E$Ak
  }

  # Empirical histogram method
  if(latent_dist=="EHM"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(all(model %in% c(1, "1PL", "Rasch", "RASCH"))){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- initialitem[,2]/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(abs(I-initialitem), na.rm = TRUE)
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # Two-component normal mixture distribution
  if(latent_dist %in% c("Mixture", "2NM")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- M1step(E, item=initialitem, model=model)
      M2 <- M2step(E)
      prob = M2$prob; d = M2$d; sd_ratio = M2$sd_ratio

      initialitem <- M1[[1]]
      if(all(model %in% c(1, "1PL", "Rasch", "RASCH"))){
        initialitem[,1] <- initialitem[,1]*M2$s
        initialitem[,2] <- initialitem[,2]/M2$s
        M1[[2]][,2] <- M1[[2]][,2]/M2$s
      }

      diff <- max(abs(I-initialitem), na.rm = TRUE)
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    Ak <- E$Ak
    density_par <- list(prob=prob, d=d, sd_ratio=sd_ratio)
  }

  # Kernel density estimation method
  if(latent_dist=="KDE"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                 range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, bandwidth=bandwidth, N=N, q=q)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(all(model %in% c(1, "1PL", "Rasch", "RASCH"))){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- initialitem[,2]/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(abs(I-initialitem), na.rm = TRUE)
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    density_par <- ld_est$par
  }

  # Davidian curve method
  if(latent_dist%in% c("DC", "Davidian")){
    density_par <- nlminb(start = rep(1,h),
                          objective = optim_phi,
                          gradient = optim_phi_grad,
                          hp=h)$par

    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model = model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, par=density_par,N=N)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density
      density_par <- ld_est$par

      if(all(model %in% c(1, "1PL", "Rasch", "RASCH"))){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- initialitem[,2]/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(abs(I-initialitem), na.rm = TRUE)
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # Log-linear smoothing method
  if(latent_dist=="LLS"){
    density_par <- rep(0, h)
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                 range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, par=density_par, N=N)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(all(model %in% c(1, "1PL", "Rasch", "RASCH"))){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- initialitem[,2]/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
        if(iter>3){
          density_par <- ld_est$par
        }
      } else {
        density_par <- ld_est$par
      }

      diff <- max(abs(I-initialitem), na.rm = TRUE)
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # ability parameter estimation
  if(is.null(ability_method)){
    theta <- NULL
    theta_se <- NULL
  } else if(ability_method == 'EAP'){
    theta <- as.numeric(E$Pk%*%E$Xk)
    theta_se <- sqrt(as.numeric(E$Pk%*%(E$Xk^2))-theta^2)
  } else if(ability_method == 'MLE'){
    mle_result <- MLE_theta(item = initialitem, data = data, type = "dich")
    theta <- mle_result[[1]]
    theta_se <- mle_result[[2]]
  }
  dn <- list(colnames(data),c("a", "b", "c"))
  dimnames(initialitem) <- dn
  # dimnames(M1[[2]]) <- dn

  # preparation for outputs

  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli_Cont(initialitem, data, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(structure(
    list(par_est=initialitem,
         # se=M1[[2]],
         fk=E$fk,
         iter=iter,
         quad=Xk,
         diff=diff,
         Ak=Ak,
         Pk=E$Pk,
         theta = theta,
         theta_se = theta_se,
         logL=-2*logL, # deviance
         density_par = density_par,
         Options = Options # specified argument values
    ),
    class = c("dich", "IRTest", "list")
  )
  )
}
