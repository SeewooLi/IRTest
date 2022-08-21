#' Title
#'
#' @param initialitem_D
#' @param initialitem_P
#' @param data_D
#' @param data_P
#' @param range
#' @param q
#' @param model_D
#' @param model_P
#' @param latent_dist
#' @param max_iter
#' @param threshold
#' @param bandwidth
#' @param h
#' @importFrom stats density nlminb
#' @importFrom utils flush.console
#' @return
#' @export
#'
#' @examples
IRTest_Mix <- function(initialitem_D, initialitem_P, data_D, data_P, range = c(-6,6),
                       q = 121, model_D, model_P="GPCM",
                       latent_dist="Normal", max_iter=200, threshold=0.0001,
                       bandwidth="nrd", h=NULL){
  Options = list(initialitem_D=initialitem_D, initialitem_P=initialitem_P,
                 data_D=data_D, data_P=data_P, range=range, q=q,
                 model_D=model_D, model_P=model_P,
                 latent_dist=latent_dist, max_iter=max_iter, threshold=threshold)
if(nrow(data_D)!=nrow(data_P)){
  warning("data_D and data_P have different number of rows.")
}else{
  I_D <- initialitem_D
  I_P <- initialitem_P
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data_D)
  bw <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Mix(item_D=initialitem_D, item_P=initialitem_P, data_D=data_D,
                     data_P=data_P, q=q, prob=0.5, d=0, sd_ratio=1, range=range)
      M1_D <- M1step(E, item=initialitem_D, model=model_D)
      M1_P <- Mstep_Poly(E, item=initialitem_P, model=model_P)
      initialitem_D <- M1_D[[1]]
      initialitem_P <- M1_P[[1]]
      diff <- max(c(max(abs(I_D-initialitem_D), na.rm = T), max(abs(I_P-initialitem_P), na.rm = T)))
      I_D <- initialitem_D
      I_P <- initialitem_P
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    Ak <- E$Ak
  }

  # Empirical histogram method
  if(latent_dist=="EHM"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      post_den <- E$fk/sum(E$fk)
      Xk <- E$Xk
      lin <- lin_inex(Xk, post_den, range = range)
      Xk <- lin$qp
      post_den <- lin$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
  }

  # Two-component normal mixture distribution
  if(latent_dist=="Mixture"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]
      M2 <- M2step(E)
      prob = M2[1];d = M2[3];sd_ratio = M2[4]
      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    Ak <- E$Ak
  }

  # Kernel density estimation method
  if(latent_dist=="KDE"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                      range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      post_den <- E$fk/sum(E$fk)
      Xk <- E$Xk
      post_den <- lin_inex(Xk, post_den, range = range)$qh
      nzindex <- round(post_den*N)!=0
      SJPI <- density(rep(Xk[nzindex], times=round(post_den*N)[nzindex]), bw = bandwidth,n=q, from = range[1], to=range[2])
      post_den <- lin_inex(Xk, SJPI$y/sum(SJPI$y), range = range)$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    bw <- c(SJPI$bw, SJPI$n)
  }

  # Davidian curve method
  if(latent_dist=="DC"){
    phipar <- nlminb(start = rep(1,h),
                     objective = optim_phi,
                     gradient = optim_phi_grad,
                     hp=h,
                     lower = -pi/2,
                     upper = pi/2)$par

    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model = model)
      initialitem <- M1[[1]]

      Xk <- E$Xk
      phipar <- nlminb(start = phipar,
                       objective = DC.LL,
                       gradient = DC.grad,
                       theta=E$Xk,
                       freq = E$fk)$par

      post_den <- dcurver::ddc(x = Xk, phi = phipar)
      post_den <- post_den/sum(post_den)
      lin <- lin_inex(Xk, post_den, range = range, rule = 2)
      Xk <- lin$qp
      post_den <- lin$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,h,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
  }
}
  # preparation for outputs
  EAP <- as.numeric(E$Pk%*%E$Xk)
  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli(item = initialitem_D, data = data_D, theta = Xk[i])+
                       logLikeli_Poly(item = initialitem_P, data = data_P, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(list(par_est=list(Dichotomous=initialitem_D,
                           Polytomous=initialitem_P),
              se=list(Dichotomous=M1_D[[2]],
                      Polytomous=M1_P[[2]]),
              fk=E$fk,
              iter=iter,
              prob=prob,
              d=d,
              sd_ratio=sd_ratio,
              quad=Xk,
              diff=diff,
              Ak=Ak,
              Pk=E$Pk,
              theta = EAP,
              logL=-2*logL, # deviance
              bw=bw,
              Options = Options # specified argument values
  ))
}
