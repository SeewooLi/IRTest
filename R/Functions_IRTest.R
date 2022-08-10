#################################################################################################################
# ICC
#################################################################################################################
P <- function(theta,a=1,b,c=0){
  c+(1-c)*(1/(1+exp(-a*(theta-b))))
}

P_P <- function(theta, a, b){
  if(length(theta)==1 & is.vector(b)){
    ps <- c(1,exp(a*cumsum(theta-b)))
    ps <- ps/sum(ps, na.rm = T)
  }else if(length(theta)==1 & is.matrix(b)){
    ps <- cbind(1,exp(t(apply(a*(theta-b),1,cumsum))))
    ps <- ps/rowSums(ps, na.rm = T)
  }else if(length(theta)!=1 & is.vector(b)){
    b <- b[!is.na(b)]
    ps <- matrix(nrow = length(theta), ncol = length(b))
    ps[,1] <- a*(theta-b[1])
    for(i in 2:length(b)){
      ps[,i] <- ps[,i-1]+a*(theta-b[i])
    }
    ps <- cbind(1,exp(ps))
    ps <- ps/rowSums(ps)
  }
  return(ps)
}

#################################################################################################################
# Likelihood
#################################################################################################################
logLikeli <- function(item, data, theta){
  if(length(theta)!=1){
    # if all of the examinees' ability values are specified
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
      for(i in 1:ncol(data)){
        for(j in 1:nrow(data)){
          L[j,i] <- data[j,i]*log(P(theta = theta[j],a=item[i,1],b=item[i,2]))+(1-data[j,i])*log(1-P(theta = theta[j],a=item[i,1],b=item[i,2]))
      }
    }
  }else{
    # if a single ability value is assumed for all
    pvector <- t(P(theta = theta,a=item[,1],b=item[,2],c=item[,3]))

    lp1 <- log(pvector)
    lp2 <- log(1-pvector)
    lp1[lp1==-Inf] <- -.Machine$double.xmax # to avoid NaN
    lp2[lp2==-Inf] <- -.Machine$double.xmax # to avoid NaN

    L <- tcrossprod(data, lp1)+tcrossprod((1-data), lp2)
    # L is an N times 1 vector, each element of which refers to an individual's log-likelihood
  }
  return(L)
}

logLikeli_Poly <- function(item, data, theta){
  pmat <- P_P(theta = theta, a = item[,1], b = item[,-1])
  N <- nrow(data)
  n <- ncol(data)
  L <- log(matrix(pmat[cbind(rep(1:n, each = N),as.vector(data)+1)], nrow = N, ncol = n))
  L[L==-Inf] <- -.Machine$double.xmax
  L <- rowSums(L)
  L[L==-Inf] <- -.Machine$double.xmax
  return(L)
}

#################################################################################################################
# Data Generation for Computer Simulation
#################################################################################################################
library(betafunctions)
DataGeneration <- function(seed=1, N=2000, nitem=10, prob=0.5, d=1.7,
                           sd_ratio=1, a_l=0.8, a_u=2.5, latent_dist="normal",
                           model=3, c_l=0, c_u=0.2){

  if(model==2){
    set.seed(seed)
    item <- matrix(c(runif(nitem,a_l,a_u),
                     sample(seq(-2,2,by=0.01),nitem, prob = dnorm(seq(-2,2,by=0.01)))), ncol=2)
    item <- round(item, digits = 2)
    initialitem <- matrix(c(rep((a_l+a_u)/2,nitem),
                            rep(0,2*nitem)), ncol=3)
  } else if(model==3){
    set.seed(seed)
    item <- matrix(c(runif(nitem,a_l,a_u),
                     sample(seq(-2,2,by=0.01),nitem, prob = dnorm(seq(-2,2,by=0.01))),
                     runif(nitem, min = c_l, max = c_u)), ncol=3)
    item <- round(item, digits = 2)
    initialitem <- matrix(c(rep((a_l+a_u)/2,nitem),
                            rep(0,2*nitem)), ncol=3)
  } else if(model=="GPCM"){
    set.seed(seed)
    item <- matrix(nrow = nitem, ncol = 7)
    item[,1] <- runif(nitem,a_l,a_u)
    for(i in 1:nitem){
      center <- rnorm(1,0,.5)
      item[i,2:(4+1)] <- sort(rnorm(4,center,.2))
    }
    initialitem <- matrix(nrow = nitem, ncol = 7)
    initialitem[,1] <- 1
    for(i in 1:nitem){
      initialitem[i,2:(4+1)] <- (-2:1+.5)/3
    }
  }

  if(latent_dist=="beta"){
    set.seed(seed)
    theta <- rBeta.4P(n=N, alpha = 3.79, beta= 10.21, l=-2.36, u=6.36)
  }else if(latent_dist=="chi"){
    set.seed(seed)
    theta <- scale(rchisq(N,df=8))
  }else if(latent_dist=="normal"){
    n1 <- round(N*prob)
    n2 <- N-n1
    m1 <- -(1-prob)*d
    m2 <- prob*d
    s1 <- sqrt((1-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
    s2 <- s1*sd_ratio

    set.seed(seed)
    theta <- c(rnorm(n=n1,mean=m1,sd=s1),rnorm(n=n2,mean=m2,sd=s2))
  } else stop("Specify type of theta dist.")
  data <- matrix(nrow = N, ncol = nitem)

  if(model==2){
    set.seed(seed)
    for(i in 1:nitem){
      for(j in 1:N){
        p <- P(theta = theta[j], a = item[i,1], b = item[i,2])
        data[j,i] <- rbinom(1,1,prob = p)
      }
    }
  }else if(model==3){
    set.seed(seed)
    for(i in 1:nitem){
      for(j in 1:N){
        p <- P(theta = theta[j], a = item[i,1], b = item[i,2], c= item[i,3])
        data[j,i] <- rbinom(1,1,prob = p)
      }
    }
  }else if(model=="GPCM"){
    set.seed(seed)
    for(i in 1:nitem){
      for(j in 1:N){
        pp <- P_P(theta = theta[j], a = item[i,1], b = item[i,-1])
        pp <- pp[!is.na(pp)]
        categ <- length(pp)-1
        data[j,i] <- sample(x = 0:categ,1,prob=pp)
      }
    }
  }

  return(list(item=item, initialitem=initialitem, theta=theta, data=data))
}


#################################################################################################################
# Initial Distribution
#################################################################################################################
dnormal <- function(x, mean=0, sd=1){
  (2*pi)^(-0.5)/sd*exp(-(x-c(mean))^2/(2*sd^2))
}


# 2C Normal Mixture Distribution
dist <- function(x, prob = 0.5, m = c(0,0), s = c(1,1)){
  prob*dnorm(x, m[1], s[1])+(1-prob)*dnorm(x, m[2], s[2])
}

# re-parameterized 2C Normal Mixture Distribution (Li, 2021)
dist2 <- function(x, prob = 0.5, d = 0, sd_ratio = 1, overallmean=0, overallsd=1){
  m1 <- -(1-prob)*d+overallmean
  m2 <- prob*d+overallmean
  s1 <- sqrt((overallsd^2-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
  s2 <- s1*sd_ratio
  density <- prob*dnormal(x, m1, s1)+(1-prob)*dnormal(x, m2, s2)
  return(density)
}

# recovering original parameters of 2C normal mixture distribution
# from re-parameterized parameters
distribution_par <- function(prob = 0.5, d = 0, sd_ratio = 1, overallmean=0, overallsd=1){
  m1 <- -(1-prob)*d+overallmean
  m2 <- prob*d+overallmean
  s1 <- sqrt((overallsd^2-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
  s2 <- s1*sd_ratio
  return(c(m1,m2,s1,s2))
}
#################################################################################################################
# E step
#################################################################################################################
Estep <- function(item, data, range = c(-4,4), q = 100, prob = 0.5, d = 0,
                  sd_ratio = 1,Xk=NULL, Ak=NULL){
  if(is.null(Xk)) {
    # quadrature points
    Xk <- seq(range[1],range[2],length=q)
  }
  if(is.null(Ak)) {
    # heights for quadrature points
    Ak <- dist2(Xk, prob, d, sd_ratio)/sum(dist2(Xk, prob, d, sd_ratio))
  }
  Pk <- matrix(nrow = nrow(data), ncol = q)
  for(i in 1:q){
    # weighted likelihood where the weight is the latent distribution
    Pk[,i] <- exp(logLikeli(item = item, data = data, theta = Xk[i]))*Ak[i]
  }
  Pk <- Pk/rowSums(Pk) # posterior weights
  rik <- crossprod(data,Pk) # observed conditional frequency of correct responses
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, rik=rik,Pk=Pk))
}

Estep_Poly <- function(item, data, range = c(-4,4), q = 100, prob = 0.5, d = 0,
                  sd_ratio = 1,Xk=NULL, Ak=NULL){
  if(is.null(Xk)) {
    # quadrature points
    Xk <- seq(range[1],range[2],length=q)
  }
  if(is.null(Ak)) {
    # heights for quadrature points
    Ak <- dist2(Xk, prob, d, sd_ratio)/sum(dist2(Xk, prob, d, sd_ratio))
  }
  Pk <- matrix(nrow = nrow(data), ncol = q)
  for(i in 1:q){
    # weighted likelihood where the weight is the latent distribution
    Pk[,i] <- exp(logLikeli_Poly(item = item, data = data, theta = Xk[i]))*Ak[i]
  }
  categ <- max(data)+1
  nitem <- nrow(item)
  Pk <- Pk/rowSums(Pk) # posterior weights
  rik <- array(dim = c(nitem, q, categ))
  for(i in 1:categ){
    rik[,,i] <- crossprod(data==i-1,Pk)
  }
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, rik=rik, Pk=Pk))
}

#################################################################################################################
# M1 step
#################################################################################################################
M1step <- function(E, item, model, max_iter=10, threshold=1e-7, EMiter){
  nitem <- nrow(item)
  item_estimated <- matrix(c(rep(1,nitem),
                             rep(0,nitem),
                             rep(0,nitem)
                             ),nrow = nrow(item), ncol = 3)
  se <- matrix(nrow = nrow(item), ncol = 3)
  X <- E$Xk
  r <- E$rik
  f <- E$fk
  ####item parameter estimation####
  for(i in 1:nitem){
    if(model[i] %in% c(1, "1PL", "Rasch", "RASCH")){

      iter <- 0
      div <- 3
      par <- item[i,2]
      ####Newton-Raphson####
      repeat{
        iter <- iter+1
        p <- P(theta = X, b=par)
        fW <- f*p*(1-p)
        diff <- as.vector(sum(r[i,]-f*p)/sum(fW))

        if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
          par <- par
        } else{
          if( sum(abs(diff)) > div){
            par <- par-div/sum(abs(diff))*diff/10
          } else {
            par <- par-diff
            div <- sum(abs(diff))
          }
        }
        if( div <= threshold | iter > max_iter) break
      }
      item_estimated[i,2] <- par
      se[i,2] <- sqrt(1/sum(fW)) # asymptotic S.E.

    } else if(model[i] %in% c(2, "2PL")){

    iter <- 0
    div <- 3
    par <- item[i,c(1,2)]
    ####Newton-Raphson####
    repeat{
      iter <- iter+1
      p <- P(theta = X, a=par[1], b=par[2])
      fp <- f*p
      fW <- fp*(1-p)
      par[1] <- max(0.1,par[1])
      X_ <- X-par[2]
      L1 <- c(sum(X_*(r[i,]-fp)),-par[1]*sum(r[i,]-fp)) #1st derivative of marginal likelihood
      d <- sum(-par[1]^2*fW)
      b <- par[1]*sum(X_*fW)
      a <- sum(-X_^2*fW)
      inv_L2 <- matrix(c(d,-b,-b,a), ncol = 2)/(a*d-b^2) #inverse of 2nd derivative of marginal likelihood
      diff <- inv_L2%*%L1
      if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
        par <- par
      } else{
        if( sum(abs(diff)) > div){
          par <- par-div/sum(abs(diff))*diff/10
        } else {
          par <- par-diff
          div <- sum(abs(diff))
        }
      }
      if( div <= threshold | iter > max_iter) break
    }
    item_estimated[i,c(1,2)] <- par
    se[i,c(1,2)] <- sqrt(-c(inv_L2[1,1], inv_L2[2,2])) # asymptotic S.E.

    } else if(model[i] %in% c(3, "3PL")){

      iter <- 0
      div <- 3
      par <- item[i,c(1,2,3)]
      ####Newton-Raphson####
      repeat{
        iter <- iter+1
        p <- P(theta = X, a=par[1], b=par[2], c=par[3])
        p_ <- P(theta = X, a=par[1], b=par[2])
        fp <- f*p
        fW <- fp*(1-p)
        W_ <- (p_*(1-p_))/(p*(1-p))
        par[1] <- max(0.1,par[1])
        par[3] <- max(0,par[3])
        X_ <- X-par[2]

        L1 <- c((1-par[3])*sum(X_*(r[i,]-fp)*W_),
                -par[1]*(1-par[3])*sum((r[i,]-fp)*W_),
                sum(r[i,]/p-f))/(1-par[3]) #1st derivative of marginal likelihood

        L2 <- diag(c(sum(-X_^2*fW*(p_/p)^2), #2nd derivative of marginal likelihood
                     -par[1]^2*sum(fW*(p_/p)^2),
                     -sum(fW/(p^2))/(1-par[3])^2
                     ))
        L2[2,1] <- par[1]*sum(X_*fW*(p_/p)^2); L2[1,2] <- L2[2,1]
        L2[3,1] <- -sum(X_*fW*p_/p^2)/(1-par[3]); L2[1,3] <- L2[3,1]
        L2[3,2] <- par[1]*sum(fW*p_/p^2)/(1-par[3]); L2[2,3] <- L2[3,2]
        inv_L2 <- solve(L2) #inverse of 2nd derivative of marginal likelihood
        diff <- inv_L2%*%L1
        if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
          par <- par
        } else{
          if( sum(abs(diff)) > div){
            par <- par-div/sum(abs(diff))*diff/10
          } else {
            par <- par-diff
            div <- sum(abs(diff))
          }
        }
        if( div <= threshold | iter > max_iter) break
      }
      par[3] <- max(0,par[3])
      item_estimated[i,c(1,2,3)] <- par
      se[i,c(1,2,3)] <- sqrt(-c(inv_L2[1,1], inv_L2[2,2], inv_L2[3,3])) # asymptotic S.E.
    } else warning("model is incorrect or unspecified.")

  }
  return(list(item_estimated, se))
}


Mstep_Poly <- function(E, item, data, model="GPCM", max_iter=3, threshold=1e-7, EMiter){
  nitem <- nrow(item)
  item_estimated <- matrix(nrow = nrow(item), ncol = 7)
  se <- matrix(nrow = nrow(item), ncol = 7)
  X <- E$Xk
  f <- E$fk
  Pk <- E$Pk
  rik <- E$rik
  N <- nrow(data)
  q <- length(X)
  ####item parameter estimation####
  for(i in 1:nitem){
    if(model %in% c("GPCM")){

      iter <- 0
      div <- 3
      par <- item[i,]
      par <- par[!is.na(par)]
      npar <- length(par)
      ####Newton-Raphson####
      repeat{
        iter <- iter+1
        par[1] <- max(0.1,par[1])
        pmat <- P_P(theta = X, a=par[1], b=par[-1])
        pcummat <- cbind(pmat[,1],pmat[,1]+pmat[,2])
        tcum <- cbind(0,X-par[2], 2*X-par[2]-par[3])
        for(j in 3:(npar-1)){
          pcummat <- cbind(pcummat, pcummat[,j-1]+pmat[,j])
          tcum <- cbind(tcum, tcum[,j]+X-par[j+1])
        }
        a_supp <- diag(tcum[,-1]%*%t(pmat[,-1]))

        # Gradients
        #Grad <- sum(Pk*t(tcum[,data[,i]+1]))-sum(f*a_supp)

        #for(j in 1:(npar-1)){
        #  Grad <- append(Grad,
        #                 par[1]*(sum(Pk[data[,i]<j,])-sum(f*pcummat[,j]))
        #                 )
        #}
        Grad <- numeric(npar)
        for(r in 1:npar){
          for(co in 1:npar){
            Grad[r] <- Grad[r]+
              sum(rik[i,,co]*PDs(probab = co, param = r, pmat,
                                pcummat, a_supp, par, tcum)/pmat[,co])
          }
        }



        # Information Matrix
        IM <- matrix(ncol = npar, nrow = npar)
        for(r in 1:npar){
          for(co in 1:r){
            ssd <- 0
            for(k in 1:npar){
              ssd <- ssd+(PDs(probab = k, param = r, pmat,
                             pcummat, a_supp, par, tcum)*
                PDs(probab = k, param = co, pmat,
                    pcummat, a_supp, par, tcum)/pmat[,k])
            }
            IM[r,co] <- f%*%ssd
            IM[co,r] <- IM[r,co]
          }
        }

        diff <- -solve(IM)%*%Grad

        if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
          par <- par
        } else{
          if( sum(abs(diff)) > div){
            par <- par-div/sum(abs(diff))*diff/10
          } else {
            par <- par-diff
            div <- sum(abs(diff))
          }
        }
        if( div <= threshold | iter > max_iter) break
      }
      item_estimated[i,1:npar] <- par
      se[i,1:npar] <- sqrt(diag(solve(IM))) # asymptotic S.E.

    } else warning("model is incorrect or unspecified.")

  }
  return(list(item_estimated, se, Grad, IM))
}

PDs <- function(probab, param, pmat, pcummat, a_supp, par, tcum){
  if(param==1){
    pmat[,probab]*(tcum[,probab]-a_supp)
  }else{
    if(param>probab){
      -par[1]*pmat[,probab]*(pcummat[,param-1]-1)
    }else if(param<=probab){
      -par[1]*pmat[,probab]*pcummat[,param-1]
    }
  }
}

#################################################################################################################
# M2 step
#################################################################################################################
M2step <- function(E, max_iter=200){
  X <- E$Xk
  f <- E$fk
  # initial values
  prob <- 0.5
  m1 <- -0.7071
  m2 <- 0.7071
  s1 <- 0.7071
  s2 <- 0.7071
  iter <- 0
  # EM algorithm - Gaussian Mixture
  repeat{
    iter <- iter+1

    resp1 <- f*(prob*dnormal(X,m1,s1))/
      (prob*dnormal(X,m1,s1)+(1-prob)*dnormal(X,m2,s2)) # responsibility
    resp2 <- f- resp1
    new_m <- c(resp1%*%X/sum(resp1),
               resp2%*%X/sum(resp2))
    diff <- m2-m1-new_m[2]+new_m[1]
    m1 <- new_m[1]
    m2 <- new_m[2]
    s1 <- sqrt(as.vector(resp1%*%(X-as.vector(m1))^2/sum(resp1)))
    s2 <- sqrt(as.vector(resp2%*%(X-as.vector(m2))^2/sum(resp2)))
    prob <- sum(resp1)/sum(f)
    if( abs(diff) < 0.0001 | iter > max_iter) break
  }
  d_raw <- m2-m1
  sd_ratio <- s2/s1
  s2total <- prob*s1^2+(1-prob)*s2^2+prob*(1-prob)*d_raw^2
  d <- d_raw/sqrt(s2total)
  return(c(prob,d_raw,d,sd_ratio,m1,m2))
}


#################################################################################################################
# Linear interpolation / extrapolation
#################################################################################################################
lin_inex <- function(qp, qh, range, rule=2){
  q <- length(qp)
  m <- qp%*%qh
  s <- (qp-c(m))^2%*%qh
  qp <- (qp-as.vector(m))/sqrt(as.vector(s))
  ap <- approx(qp, y = qh, xout = seq(range[1],range[2],length=q),method = "linear", rule=rule)
  return(list(qp=ap$x, qh=ap$y/sum(ap$y)))
}


#################################################################################################################
# Estimation
#################################################################################################################
IRTest_Dich <- function(initialitem, data, range = c(-6,6), q = 121, model,
                         latent_dist="Normal", max_iter=200, threshold=0.0001,
                         bandwidth="nrd", h=NULL){
  Options = list(initialitem=initialitem, data=data, range=range, q=q, latent_dist=latent_dist, max_iter=max_iter, threshold=threshold)
  I <- initialitem
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data)
  bw <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range)
      M1 <- M1step(E, item=initialitem, model=model)
      initialitem <- M1[[1]]
      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
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

      E <- Estep(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- M1step(E, item=initialitem, model=model)
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

      E <- Estep(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                 range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model=model)
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

      E <- Estep(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model = model)
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

  # preparation for outputs
  EAP <- as.numeric(E$Pk%*%E$Xk)
  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli(initialitem, data, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(list(par_est=initialitem,
              se=M1[[2]],
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

IRTest_Poly <- function(initialitem, data, range = c(-6,6), q = 121, model,
                        latent_dist="Normal", max_iter=200, threshold=0.0001,
                        bandwidth="nrd", h=NULL){
  Options = list(initialitem=initialitem, data=data, range=range, q=q, latent_dist=latent_dist, max_iter=max_iter, threshold=threshold)
  I <- initialitem
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data)
  bw <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range)
      M1 <- Mstep_Poly(E, item=initialitem, model=model, data=data)
      initialitem <- M1[[1]]
      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
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

      E <- Estep(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- M1step(E, item=initialitem, model=model)
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
      M1 <- Mstep_Poly(E, item=initialitem, model=model, data=data)
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

      E <- Estep(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- M1step(E, item=initialitem, model = model)
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

  # preparation for outputs
  EAP <- as.numeric(E$Pk%*%E$Xk)
  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli_Poly(initialitem, data, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(list(par_est=initialitem,
              se=M1[[2]],
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


#################################################################################################################
# Plotting
#################################################################################################################
plot_LD <- function(model, from = -6, to=6, add = F){
  if(model[["Options"]][["latent_dist"]]=="Mixture"){
    curve(dist2(x,prob = model$prob, d=model$d, sd_ratio = model$sd_ratio),
          from = from, to=to, add = add, ylab="latent density", xlab=expression(theta))
  } else {
    if(model[["Options"]][["latent_dist"]]=="Normal"){
      curve(dnormal(x),
            from = from, to=to, add = add, ylab="latent density", xlab=expression(theta))
    }else{
      plot(model$quad,model$Ak*(1/(model$quad[2]-model$quad[1])),
           type = "l", ylab="latent density", xlab=expression(theta))
    }
  }
}
