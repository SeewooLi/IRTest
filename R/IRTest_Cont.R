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

      diff <- max(abs(I-initialitem), na.rm = TRUE)
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

      E <- Estep(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range, Xk=Xk, Ak=Ak)
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

      E <- Estep(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
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

      E <- Estep(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
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

      E <- Estep(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
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

      E <- Estep(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
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
