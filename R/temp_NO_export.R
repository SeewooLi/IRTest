###############################################################################
# ICC
###############################################################################
P <- function(theta,a=1,b,c=0){
  c+(1-c)*(1/(1+exp(-a*(theta-b))))
}


###############################################################################
# Likelihood
###############################################################################
logLikeli <- function(item, data, theta, model){
  if(length(theta)!=1){
    # if all of the examinees' ability values are specified
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
    for(i in 1:ncol(data)){
      for(j in 1:nrow(data)){
        L[j,i] <- data[j,i]*log(P(theta = theta[j],a=item[i,1],b=item[i,2]))+
          (1-data[j,i])*log(1-P(theta = theta[j],a=item[i,1],b=item[i,2]))
      }
    }
  }else{
    # if a single ability value is assumed for all
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
    pvector <- t(P(theta = theta,a=item[,1],b=item[,2],c=item[,3]))

    lp1 <- log(pvector)
    lp2 <- log(1-pvector)
    lp1[lp1==-Inf] <- -.Machine$double.xmax # to avoid NaN
    lp2[lp2==-Inf] <- -.Machine$double.xmax # to avoid NaN

    L <- tcrossprod(data, lp1)+tcrossprod((1-data), lp2)
    # L is an N times 1 vector,
    # each element of which refers to an individual's log-likelihood
  }
  return(L)
}

###############################################################################
# E step
###############################################################################
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
gamma_jk <- function(i){
  Pk[,i] <- exp(logLikeli(item = item, data = data, theta = Xk[i]))*Ak[i]
}

##############################################################################
# M1 step
##############################################################################
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
      se[i,c(1,2,3)] <- sqrt(-c(inv_L2[1,1],
                                inv_L2[2,2], inv_L2[3,3])) # asymptotic S.E.
    } else warning("model is incorrect or unspecified.")

  }
  return(list(item_estimated, se))
}

###############################################################################
# M2 step
###############################################################################
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


##############################################################################
# Linear interpolation / extrapolation
##############################################################################
lin_inex <- function(qp, qh, range, rule=2){
  q <- length(qp)
  m <- qp%*%qh
  s <- (qp-c(m))^2%*%qh
  qp <- (qp-as.vector(m))/sqrt(as.vector(s))
  ap <- approx(qp, y = qh, xout = seq(range[1],range[2],length=q),
               method = "linear", rule=rule)
  return(list(qp=ap$x, qh=ap$y/sum(ap$y)))
}


##############################################################################
# B matrix
##############################################################################
B_mat <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(sqrt(eigen(mmat)$values))%*%t(eigen(mmat)$vectors)
  return(umat)
}
B_mat2 <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(svd(mmat)$d^.25)%*%t(svd(mmat)$v)
  return(umat)
}

##############################################################################
# c vector
##############################################################################
c_vec <- function(phi){
  h <- length(phi)
  cvec <- numeric(h+1)
  cvec[1] <- sin(phi[1])
  if(h>1){
    for(i in 2:h){
      cvec[i] <- prod(cos(phi[1:(i-1)]))*sin(phi[i])
    }
  }
  cvec[h+1] <- prod(cos(phi))
  return(cvec)
}

##############################################################################
# Z vector
##############################################################################
Z_vec <- function(theta, h){
  theta^(0:h)
}

##############################################################################
# polynomial
##############################################################################
DC_poly <- function(phi, theta, invB, cvec){
  h <- length(phi)
  m <- invB%*%cvec
  dcpoly <- as.vector(t(m)%*%sapply(theta,Z_vec,h=h))
  return(dcpoly)
}

##############################################################################
# Davidian curve
##############################################################################
dDC <- function(phi, theta){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  densDC <- (DC_poly(phi, theta, invB, cvec))^2*dnormal(theta)
  return(densDC)
}

##############################################################################
# log-likelihood for DC
##############################################################################
LLDC <- function(phi, theta, freq){
  -freq%*%log(dDC(phi, theta))
}

##############################################################################
# c-phi matrix
##############################################################################
c_phi <- function(phi, cvec){
  h <- length(phi)
  cphi <- matrix(data = 0, nrow = h+1, ncol = h)
  diagphi <- cvec[1:h]/tan(phi)
  diagphi[is.nan(diagphi)] <- 0
  diag(cphi) <- diagphi
  if(h > 1){
    for(i in 2:h){
      cphi[i,1:(i-1)] <- -cvec[i]*tan(phi[1:(i-1)])
    }
  }
  cphi[h+1,] <- -cvec[h+1]*tan(phi)
  return(cphi)
}

##############################################################################
# 1st derivative
##############################################################################
grad_DC <- function(phi, theta, freq){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  matalg <- t(c_phi(phi, cvec))%*%t(invB)%*%sapply(c(theta),Z_vec,h=h)
  dv1 <- -(2*freq/DC_poly(phi, theta, invB, cvec))%*%t(matalg)
  return(dv1)
}

##############################################################################
# Initial phi
##############################################################################
optim_phi <- function(phi, hp){
  target <- as.vector(B_mat(hp)%*%rep(1,times=hp+1))
  obj <- sum((c_vec(phi)-target)^4)
  return(obj)
}
optim_phi_grad <- function(phi, hp){
  target <- as.vector(B_mat(hp)%*%rep(1,times=hp+1))
  cvec <- c_vec(phi)
  obj <- as.vector(4*(c_vec(phi)-target)^3%*%c_phi(phi,cvec))
  return(obj)
}

##############################################################################
# Importing pdf and gradient functions from "dcurver" package
##############################################################################

DC.LL <- function (phi, theta, freq) {
  LL <- sum(freq * log(dcurver::ddc(theta, phi)))
  -LL
}
DC.grad <- function (phi, theta, freq) {
  -colSums(dcurver::dc_grad(theta, phi) * freq)
}
