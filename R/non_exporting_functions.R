#################################################################################################################
# citation
#################################################################################################################
.onAttach <- function(libname, pkgname) {
  package_citation <- "Li, S. (2024). IRTest: Parameter estimation of item response theory with estimation of latent distribution (Version 2.1.0). R package. \n"
  package_URL <- "URL: https://CRAN.R-project.org/package=IRTest"
  packageStartupMessage("Thank you for using IRTest!")
  packageStartupMessage("Please cite the package as: \n")
  packageStartupMessage(package_citation)
  packageStartupMessage(package_URL)
}


#################################################################################################################
# ICC
#################################################################################################################
P <- function(theta,a=1,b,c=0){
  c+(1-c)*(1/(1+exp(-a*(theta-b))))
}

P2 <- function(theta,b,a=1){
  1/(1+exp(-a*(theta-b)))
}

P_P <- function(theta, a, b){
  if(length(theta)==1 & is.vector(b)){
    if(length(a)==1){
      ps <- c(1,exp(a*cumsum(theta-b)))
      ps <- ps/sum(ps, na.rm = T)
    } else {
      ps <- cbind(1,exp(a*(theta-matrix(b))))
      ps <- ps/rowSums(ps, na.rm = T)
    }
  }else if(length(theta)==1 & is.matrix(b)){
    ps <- cbind(1,exp(t(apply(a*(theta-b),1,cumsum))))
    ps <- ps/rowSums(ps, na.rm = T)
  }else if(length(theta)!=1 & is.vector(b)){
    b <- b[!is.na(b)]
    ps <- matrix(nrow = length(theta), ncol = length(b))
    ps[,1] <- a*(theta-b[1])
    if(length(b)>1){
      for(i in 2:length(b)){
        ps[,i] <- ps[,i-1]+a*(theta-b[i])
      }
    }
    ps <- cbind(1,exp(ps))
    ps <- ps/rowSums(ps)
  }
  return(ps)
}

P_G <- function(theta, a, b){
  if(length(theta)==1 & is.vector(b)){
    b <- b[!is.na(b)]
    if(length(a)==1){
      ps <- P(theta = theta, a = a, b = b)
      ps <- c(1, ps) - c(ps, 0)
    } else {
      ps <- cbind(1,exp(a*(theta-matrix(b))))
      ps <- ps/rowSums(ps, na.rm = T)
    }
  }else if(length(theta)==1 & is.matrix(b)){
    ps <- P(theta = theta, a = a, b = b)
    ps <- cbind(1, ps)-add0(cbind(ps, NA))
  }else if(length(theta)!=1 & is.vector(b)){
    b <- b[!is.na(b)]
    ps <- outer(X=theta, Y=b, FUN = P2, a=a)
    ps <- cbind(1, ps)-cbind(ps, 0)
  }
  return(ps)
}

add0 <- function(x){
  x[cbind(1:nrow(x),rowSums(!is.na(x))+1)] <- 0
  return(x)
}

likert <- function(theta, a, b, nu, ncats=5, cut_score=NULL){
  p <- P(theta = theta, a = a, b = b)
  if(is.null(cut_score) & is.null(ncats)){
    stop("Specify either ncat or cut_score.")
  }else if(is.null(cut_score)){
    cut_score <- (1:(ncats-1))/ncats
  }else{
    ncats <- length(cut_score)+1
  }

  if(is.matrix(cut_score)){
    cut_score <- t(apply(cut_score, MARGIN=1, FUN=cut_trans))
    probs <- matrix(nrow = nrow(cut_score), ncol = ncol(cut_score))
    for(i in 1:ncol(cut_score)){
      probs[,i] <- pbeta(q = cut_score[,i],
                         shape1 = p*nu,
                         shape2 = (1-p)*nu)
    }
    return(add1(cbind(probs,NA))-cbind(0,probs))
  }else if(length(theta)==1 & length(b)!=1){
    probs <- matrix(nrow = length(b), ncol = ncats-1)

    for(i in 1:length(cut_score)){
      probs[,i] <- pbeta(q = cut_score[i],
                         shape1 = p*nu,
                         shape2 = (1-p)*nu)
    }
    return(cbind(probs,1)-cbind(0,probs))
  }else if(length(theta)==1){
    probs <- pbeta(q = cut_score,
                   shape1 = p*nu,
                   shape2 = (1-p)*nu)
    return(c(probs,1)-c(0,probs))
  }else if(length(theta)>1){
    probs <- matrix(nrow = length(theta), ncol = ncats-1)

    for(i in 1:length(cut_score)){
      probs[,i] <- pbeta(q = cut_score[i],
                         shape1 = p*nu,
                         shape2 = (1-p)*nu)
    }
    return(cbind(probs,1)-cbind(0,probs))
  }
}

add1 <- function(x){
  x[cbind(1:nrow(x),rowSums(!is.na(x))+1)] <- 1
  return(x)
}

cut_trans <- function(x){
  ind <- !is.na(x)
  return(
    cumsum(
      c(1,exp(x[-length(x[ind])]))/sum(c(1,exp(x[ind])))
    )
  )
}
#################################################################################################################
# Distribution
#################################################################################################################
dnormal <- function(x, mean=0, sd=1){
  (2*pi)^(-0.5)/sd*exp(-(x-c(mean))^2/(2*sd^2))
}


# # 2C Normal Mixture Distribution
# dist <- function(x, prob = 0.5, m = c(0,0), s = c(1,1)){
#   prob*dnormal(x, m[1], s[1])+(1-prob)*dnormal(x, m[2], s[2])
# }

#################################################################################################################
# Reordering Scores for polytomous data
#################################################################################################################
extract_cat <- function(x){
  sort(unique(x))
}

reorder_vec <- function(x){
  match(x, table = extract_cat(x))-1
}

reorder_mat <- function(x){
  apply(x, MARGIN = 2, FUN = reorder_vec)
}

#################################################################################################################
# Likelihood
#################################################################################################################
logLikeli <- function(item, data, theta){
  data2 <- 1-data
  data[is.na(data)] <- 0
  data2[is.na(data2)] <- 0
  if(length(theta)!=1){
    # if all of the examinees' ability values are specified
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
    for(i in 1:ncol(data)){
      for(j in 1:nrow(data)){
        L[j,i] <- data[j,i]*log(P(theta = theta[j],a=item[i,1],b=item[i,2]))+(data2[j,i])*log(1-P(theta = theta[j],a=item[i,1],b=item[i,2]))
      }
    }
  }else{
    # if a single ability value is assumed for all
    pvector <- t(P(theta = theta,a=item[,1],b=item[,2],c=item[,3]))

    lp1 <- log(pvector)
    lp2 <- log(1-pvector)
    lp1[lp1==-Inf] <- -.Machine$double.xmax # to avoid NaN
    lp2[lp2==-Inf] <- -.Machine$double.xmax # to avoid NaN

    L <- tcrossprod(data, lp1)+tcrossprod(data2, lp2)
    # L is an N times 1 vector, each element of which refers to an individual's log-likelihood
  }
  return(L)
}

logLikeli_Poly <- function(item, data, theta, model, ncats=NULL, ab_par=NULL){
  b_pars <- if(nrow(item)==1) item[,-1,drop=FALSE] else item[,-1]
  if(model %in% c("PCM", "GPCM")){
    pmat <- P_P(theta = theta, a = item[,1], b = b_pars)
  } else if(model == "GRM"){
    pmat <- P_G(theta = theta, a = item[,1], b = b_pars)
  } else if(model == "likert"){
    pmat <- likert(theta = theta, a = item[,1], b = item[,2], nu = exp(item[,3]), ncats=ncats)
  } else if(model == "likert2"){
    pmat <- likert(theta = theta, a = ab_par[,1], b = ab_par[,2], nu = exp(item[,1]), cut_score = item[,-1])
  }

  L <- NULL
  for(i in 1:nrow(item)){
    L <- cbind(L, pmat[i,][data[,i]+1])
  }
  L <- rowSums(log(L),na.rm = TRUE)
  # L <- rowSums(
  #   log(
  #     matrix(
  #       pmat[cbind(rep(1:ncol(data), each = nrow(data)),as.vector(data)+1)],
  #       nrow = nrow(data),
  #       ncol = ncol(data)
  #       )
  #     ),
  #   na.rm = TRUE
  #   )
  L[L==-Inf] <- -.Machine$double.xmax
  return(L)
}

#' @importFrom stats dbeta
#'
logLikeli_Cont <- function(item, data, theta){
  p <- P(theta = theta, a = item[,1], b = item[,2])

  L <- NULL
  for(i in 1:nrow(item)){
    L <- cbind(L, dbeta(x = data[,i],
                        shape1 = p[i]*item[i,3],
                        shape2 = (1-p[i])*item[i,3],
                        log = TRUE)
               )
  }
  L <- rowSums(L, na.rm = TRUE)
  L[L==-Inf] <- -.Machine$double.xmax
  return(L)
}

# log_P_cont <- function(response, theta, a, b, phi){
#   mu <- P(theta, a, b)
#   return(dbeta(x=response, shape1 = mu*phi, shape2 = (1-mu)*phi, log = TRUE))
# }
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
  rik <- array(dim = c(nrow(item), q, 2)) # observed conditional frequency of correct responses
  for(i in 1:2){
    d_ <- data==(i-1)
    d_[is.na(d_)] <- 0
    rik[,,i] <- crossprod(d_,Pk)
  }
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, rik_D=rik,Pk=Pk))
}

Estep_Poly <- function(item, data, range = c(-4,4), q = 100, prob = 0.5, d = 0,
                       sd_ratio = 1,Xk=NULL, Ak=NULL, model, ncats=NULL, ab_par=NULL){
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
    Pk[,i] <- exp(logLikeli_Poly(item = item, data = data, theta = Xk[i], model, ncats, ab_par))*Ak[i]
  }
  categ <- max(data, na.rm = TRUE)+1
  Pk <- Pk/rowSums(Pk) # posterior weights
  rik <- array(dim = c(nrow(item), q, categ))
  for(i in 1:categ){
    d_ <- data==(i-1)
    d_[is.na(d_)] <- 0
    rik[,,i] <- crossprod(d_,Pk)
  }
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, rik_P=rik, Pk=Pk))
}

Estep_Mix <- function(item_D, item_P, data_D, data_P, range = c(-4,4), q = 100, prob = 0.5, d = 0,
                       sd_ratio = 1,Xk=NULL, Ak=NULL, model){
  if(is.null(Xk)) {
    # quadrature points
    Xk <- seq(range[1],range[2],length=q)
  }
  if(is.null(Ak)) {
    # heights for quadrature points
    Ak <- dist2(Xk, prob, d, sd_ratio)/sum(dist2(Xk, prob, d, sd_ratio))
  }
  Pk <- matrix(nrow = nrow(data_D), ncol = q)
  for(i in 1:q){
    # weighted likelihood where the weight is the latent distribution
    Pk[,i] <- exp(logLikeli(item = item_D, data = data_D, theta = Xk[i])+
                    logLikeli_Poly(item = item_P, data = data_P, theta = Xk[i], model))*Ak[i]
  }
  categ <- max(data_P, na.rm = TRUE)+1
  Pk <- Pk/rowSums(Pk) # posterior weights
  rik_D <- array(dim = c(nrow(item_D), q, 2)) # observed conditional frequency of correct responses
  for(i in 1:2){
    d_ <- data_D==(i-1)
    d_[is.na(d_)] <- 0
    rik_D[,,i] <- crossprod(d_,Pk)
  }
  rik_P <- array(dim = c(nrow(item_P), q, categ))
  for(i in 1:categ){
    d_ <- data_P==(i-1)
    d_[is.na(d_)] <- 0
    rik_P[,,i] <- crossprod(d_,Pk)
  }
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, rik_D=rik_D, rik_P=rik_P, Pk=Pk))
}

Estep_Cont <- function(item, data, range = c(-4,4), q = 81, prob = 0.5, d = 0,
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
    Pk[,i] <- exp(logLikeli_Cont(item = item, data = data, theta = Xk[i]))*Ak[i]
  }
  Pk <- Pk/rowSums(Pk) # posterior weights
  # rik <- crossprod(data, t(t(Pk)/rowSums(t(Pk))), na.rm=TRUE)
  fk <- colSums(Pk) # expected frequency of examinees
  return(list(Xk=Xk, Ak=Ak, fk=fk, Pk=Pk))
}
#################################################################################################################
# M1 step
#################################################################################################################
M1step <- function(E, item, model, max_iter=5, threshold=1e-7, EMiter){
  nitem <- nrow(item)
  item_estimated <- item
  se <- matrix(nrow = nrow(item), ncol = 3)
  X <- E$Xk
  r <- E$rik_D
  ####item parameter estimation####
  for(i in 1:nitem){
    f <- rowSums(r[i,,])
    if(sum(f)==0){
      item_estimated[i,2] <- NA
    } else {
      if(model[i] %in% c(1, "1PL", "Rasch", "RASCH")){

        iter <- 0
        div <- 3
        par <- item[i,2]
        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          p <- P(theta = X, a = item[i,1], b=par)
          fW <- f*p*(1-p)
          diff <- as.vector(sum(r[i,,2]-f*p)/sum(fW)/item[i,1])

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
            warning("Infinite or `NA` estimates produced.")
          } else{
            if( sum(abs(diff)) > div){
              par <- par-div/sum(abs(diff))*diff/2
            } else {
              par <- par-diff
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,2] <- par
        se[i,2] <- sqrt(1/sum(fW)/item[i,1]^2) # asymptotic S.E.

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
          X_ <- X-par[2]
          L1 <- c(sum(X_*(r[i,,2]-fp)),-par[1]*sum(r[i,,2]-fp)) #1st derivative of marginal likelihood
          d <- sum(-par[1]^2*fW)
          b <- par[1]*sum(X_*fW)
          a <- sum(-X_^2*fW)
          inv_L2 <- matrix(c(d,-b,-b,a), ncol = 2)/(a*d-b^2) #inverse of 2nd derivative of marginal likelihood
          diff <- inv_L2%*%L1
          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
            warning("Infinite or `NA` estimates produced.")
          } else{
            if( sum(abs(diff)) > div){
              if(max(abs(diff[-1]))/abs(diff[1])>1000){
                par <- -par
              } else{
                par <- par-div/sum(abs(diff))*diff/2
              }
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
          par[3] <- max(0,par[3])
          X_ <- X-par[2]

          L1 <- c((1-par[3])*sum(X_*(r[i,,2]-fp)*W_),
                  -par[1]*(1-par[3])*sum((r[i,,2]-fp)*W_),
                  sum(r[i,,2]/p-f))/(1-par[3]) #1st derivative of marginal likelihood

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
            warning("Infinite or `NA` estimates produced.")
          } else{
            if( sum(abs(diff)) > div){
              if(max(abs(diff[-1]))/abs(diff[1])>1000){
                par <- -par
              } else{
                par <- par-div/sum(abs(diff))*diff/2
              }
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
  }
  return(list(item_estimated, se))
}


Mstep_Poly <- function(E, item, model="GPCM", max_iter=5, threshold=1e-7, EMiter, ncats=NULL, ab_par=NULL){
  nitem <- nrow(item)
  item_estimated <- item
  se <- matrix(nrow = nrow(item), ncol = ncol(item))
  X <- E$Xk
  Pk <- E$Pk
  rik <- E$rik_P
  N <- nrow(Pk)
  q <- length(X)

  if(any(model %in% c("likert","likert2"))){
    if(length(ncats)==1){
      ncats <- rep(ncats, nitem)
    }
    if(any(model %in% c("likert"))){
      ngrid <- 100
    }else if(any(model %in% c("likert2"))){
      ngrid <- 1000
    }
    grids <- seq(1/2/ngrid, 1-1/2/ngrid, length=ngrid)
  }
  ####item parameter estimation####
  for(i in 1:nitem){
    if(sum(rik[i,,])!=0){
      if(model %in% c("PCM")){
        iter <- 0
        div <- 3
        par <- item[i,]
        par <- par[!is.na(par)]
        npar <- length(par)
        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          pmat <- P_P(theta = X, a=par[1], b=par[-1])
          pcummat <- cbind(pmat[,1])
          tcum <- cbind(0,X-par[2])
          if(npar>2){
            for(j in 2:(npar-1)){
              pcummat <- cbind(pcummat, pcummat[,j-1]+pmat[,j])
              tcum <- cbind(tcum, tcum[,j]+X-par[j+1])
            }
          }
          a_supp <- NULL # diag(tcum[,-1]%*%t(pmat[,-1]))

          # Gradients
          Grad <- numeric(npar-1)

          # Information Matrix
          IM <- matrix(ncol = npar-1, nrow = npar-1)

          for(r in 2:npar){
            for(co in 1:npar){
              Grad[r-1] <- Grad[r-1]+
                sum(rik[i,,co]*PDs(probab = co, param = r, pmat,
                                   pcummat, a_supp, par, tcum)/pmat[,co])
            }
          }
          for(r in 2:npar){
            for(co in 2:npar){
              if(r >= co){
                ssd <- 0
                for(k in 1:npar){
                  ssd <- ssd+(PDs(probab = k, param = r, pmat,
                                  pcummat, a_supp, par, tcum)*
                                PDs(probab = k, param = co, pmat,
                                    pcummat, a_supp, par, tcum)/pmat[,k])
                }
                IM[r-1,co-1] <- rowSums(rik[i,,])%*%ssd
                IM[co-1,r-1] <- IM[r-1,co-1]
              }
            }
          }

          diff <- -solve(IM)%*%Grad

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
          } else{
            if( sum(abs(diff)) > div){
              par <- par-c(0, div/sum(abs(diff))*diff/2)
            } else {
              par <- par-c(0, diff)
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,1:npar] <- par
        se[i,1:npar] <- c(NA, sqrt(diag(solve(IM)))) # asymptotic S.E.

      } else if(model %in% c("GPCM")){

        iter <- 0
        div <- 3
        par <- item[i,]
        par <- par[!is.na(par)]
        npar <- length(par)
        f <- rowSums(rik[i,,])
        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          # par[1] <- max(0.1,par[1])
          pmat <- P_P(theta = X, a=par[1], b=par[-1])
          pcummat <- cbind(pmat[,1])
          tcum <- cbind(0,X-par[2])
          if(npar>2){
            for(j in 2:(npar-1)){
              pcummat <- cbind(pcummat, pcummat[,j-1]+pmat[,j])
              tcum <- cbind(tcum, tcum[,j]+X-par[j+1])
            }
          }
          a_supp <- diag(tcum[,-1]%*%t(pmat[,-1]))

          # Gradients
          Grad <- numeric(npar)

          # Information Matrix
          IM <- matrix(ncol = npar, nrow = npar)
          for(r in 1:npar){
            for(co in 1:npar){
              Grad[r] <- Grad[r]+
                sum(rik[i,,co]*PDs(probab = co, param = r, pmat,
                                   pcummat, a_supp, par, tcum)/pmat[,co])
              if(r >= co){
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
          }

          diff <- -solve(IM)%*%Grad

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
          } else{
            if( sum(abs(diff)) > div){
              if(max(abs(diff[-1]))/abs(diff[1])>1000){
                par <- -par
              } else{
                par <- par-div/sum(abs(diff))*diff/2
              }
            } else {
              par <- par-diff
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,1:npar] <- par
        se[i,1:npar] <- sqrt(diag(solve(IM))) # asymptotic S.E.

      } else if(model %in% c("GRM")){

        iter <- 0
        div <- 3
        par <- item[i,]
        par <- par[!is.na(par)]
        npar <- length(par)
        f <- rowSums(rik[i,,])
        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          pmat <- P_G(theta = X, a=par[1], b=par[-1])
          p_ <- outer(X = X, Y = par[-1], FUN = P2, a=par[1])
          X_ <- cbind(0, outer(X = X, Y = par[-1], FUN="-"), 0)
          ws <- cbind(0, p_*(1-p_), 0)
          # Gradients
          Grad <- numeric(npar)

          # Information Matrix
          IM <- matrix(0, ncol = npar, nrow = npar)

          IM[1,1] <- -f%*%((X_[,1]*ws[,1]-X_[,2]*ws[,2])^2/pmat[,1])
          Grad[1] <- sum(rik[i,,1]*(X_[,1]*ws[,1]-X_[,2]*ws[,2])/pmat[,1])

          for(k in 2:npar){
            Grad[1] <- Grad[1] + sum(
              rik[i,,k]*(X_[,k]*ws[,k]-X_[,k+1]*ws[,k+1])/pmat[,k]
              )
            IM[1,1] <- IM[1,1]-f%*%((X_[,k]*ws[,k]-X_[,k+1]*ws[,k+1])^2/pmat[,k])

            Grad[k] <- sum(
              par[1]*ws[,k]*(rik[i,,k-1]/pmat[,k-1] -rik[i,,k]/pmat[,k])
            )
            IM[1,k] <- -par[1]*f%*%(
              ws[,k]*(
                (X_[,k-1]*ws[,k-1]-X_[,k]*ws[,k])/pmat[,k-1]-
                  (X_[,k]*ws[,k]-X_[,k+1]*ws[,k+1])/pmat[,k]
                )
              )
            IM[k,k] <- -par[1]^2*f%*%(ws[,k]^2*(1/pmat[,k-1]+1/pmat[,k]))
            if(k<npar){
              IM[k,k+1] <- par[1]^2*f%*%(ws[,k]*ws[,k+1]/pmat[,k])
            }
          }


          for(r in 1:npar){
            for(co in 1:npar){
              if(r < co){
                IM[co,r] <- IM[r,co]
              }
            }
          }

          diff <- solve(IM)%*%Grad

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
          } else{
            if( sum(abs(diff)) > div){
              if(max(abs(diff[-1]))/abs(diff[1])>1000){
                par <- -par
              } else{
                par <- par-div/sum(abs(diff))*diff/2
              }
            } else {
              par <- par-diff
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,1:npar] <- par
        se[i,1:npar] <- sqrt(-diag(solve(IM))) # asymptotic S.E.

      } else if(model %in% c("likert")){
        iter <- 0
        div <- 3
        par <- item[i,]
        f <- rowSums(rik[i,,])

        cut_score <- (1:(ncats[i]-1))/ncats[i]
        ind_cat <- as.numeric(cut(grids,breaks = c(0,cut_score,1),labels = 1:ncats[i]))
        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          mu <- P(X, par[1], par[2])
          nu <- exp(par[3])
          p0 <- likert(X, a = par[1], b = par[2], nu = nu, ncats = ncats[i])

          pmat <- t(outer(grids, nu*mu-1, FUN = "^")*outer(1-grids, nu*(1-mu)-1, FUN = "^"))/beta(nu*mu,nu*(1-mu)) # probability matrix wo the normalizing factor
          l1mu <- nu*pmat*t(outer(log(grids/(1-grids)), digamma(nu*mu)-digamma(nu*(1-mu)), FUN = "-"))
          l1xi <- nu*(digamma(nu)+
                        mu*t(outer(log(grids), digamma(nu*mu), FUN = "-"))+
                        (1-mu)*t(outer(log(1-grids), digamma(nu*(1-mu)), FUN = "-"))
          )*pmat
          l1m <- matrix(ncol = ncats[i], nrow = q)
          l1x <- matrix(ncol = ncats[i], nrow = q)
          for(c in 1:ncats[i]){
            l1m[,c] <- rowSums(l1mu[,ind_cat==c])*0.01
            l1x[,c] <- rowSums(l1xi[,ind_cat==c])*0.01
          }
          # l1m <- l1m
          # l1x <- nu/beta(nu*mu,nu*(1-mu))*l1x
          l1a <- (X-par[2])*mu*(1-mu)*l1m
          l1b <- -par[1]*mu*(1-mu)*l1m

          Grad <- c(
            sum(l1a*rik[i,,]/p0),
            sum(l1b*rik[i,,]/p0),
            sum(l1x*rik[i,,]/p0)
          )

          L2aa <- rowSums(l1a^2  /p0)%*%f
          L2ab <- rowSums(l1a*l1b/p0)%*%f
          L2bb <- rowSums(l1b^2  /p0)%*%f
          L2xi <- rowSums(l1x^2  /p0)%*%f
          L2ax <- rowSums(l1a*l1x/p0)%*%f
          L2bx <- rowSums(l1b*l1x/p0)%*%f

          IM <- solve(matrix(data = c(
            L2aa, L2ab, L2ax,
            L2ab, L2bb, L2bx,
            L2ax, L2bx, L2xi
          ), nrow = 3))

          diff <- -IM%*%Grad

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
          } else{
            if( sum(abs(diff)) > div){
              if((par[1]-diff[1]/2)<0){
                par <- par - diff/sum(abs(diff))
              }else{
                par <- par - diff/2
              }
            } else {
              par <- par - diff
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,] <- par
        se[i,] <- sqrt(diag(IM)) # asymptotic S.E.

      } else if(model %in% c("likert2")){
        iter <- 0
        div <- 1
        par <- item[i,]
        par <- par[!is.na(par)]
        npar <- length(par)
        ncut <- npar-1
        ab <- ab_par[i,]
        f <- rowSums(rik[i,,])

        ####Newton-Raphson####
        repeat{
          iter <- iter+1
          cut_score <- cut_trans(par[-1])
          ind_cat <- as.numeric(cut(grids,breaks = c(0,cut_score,1),labels = 1:npar))

          mu <- P(X, ab[1], ab[2])
          nu <- exp(par[1])
          p0 <- likert(X, a = ab[1], b = ab[2], nu = nu, cut_score = cut_score)

          beta_vals <- matrix(nrow = q, ncol=ncut)
          for(b in 1:ncut){
            beta_vals[,b] <- dbeta(cut_score[b], shape1 = nu*mu, shape2 = nu*(1-mu))
          }

          l1s <- list()

          # nu
          pmat <- t(outer(grids, nu*mu-1, FUN = "^")*outer(1-grids, nu*(1-mu)-1, FUN = "^"))/beta(nu*mu,nu*(1-mu)) # probability matrix wo the normalizing factor
          l1xi <- nu*(digamma(nu)+
                        mu*t(outer(log(grids), digamma(nu*mu), FUN = "-"))+
                        (1-mu)*t(outer(log(1-grids), digamma(nu*(1-mu)), FUN = "-"))
          )*pmat
          l1x <- matrix(ncol = npar, nrow = q)
          for(c in 1:(npar)){
            l1x[,c] <- rowSums(l1xi[,ind_cat==c])/ngrid
          }
          l1s[[1]] <- l1x

          # thresholds
          for(b in 1:ncut){
            ind <- c(rep(0,b),rep(1,ncut-b))
            temp <- t((ind-cut_score)*t(beta_vals))*cut_score[1]*exp(par[b+1])
            l1s[[b+1]] <- cbind(temp,0)-cbind(0,temp)
          }

          # derivatives
          L1 <- c()
          L2 <- matrix(nrow = npar, ncol = npar)
          for(r in 1:npar){
            L1 <- append(
              L1,
              sum(l1s[[r]]*rik[i,,1:npar]/p0)
            )
            for(c in 1:npar){
              if(r>=c){
                L2[r,c] <- rowSums(l1s[[r]]*l1s[[c]]/p0)%*%f
                L2[c,r] <- L2[r,c]
              }
            }
          }
          # L1[1] <- L1[1]+par[1]-2
          # L2[1,1] <- L2[1,1]+1
          IM <- solve(L2)
          diff <- L1%*%IM

          if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
            par <- par
          } else{
            if( sum(abs(diff)) > div){
              stepsize <- max(max(abs(diff)),2)
              par <- par + diff/stepsize
            } else {
              par <- par + diff
              div <- sum(abs(diff))
            }
          }
          if( div <= threshold | iter > max_iter) break
        }
        item_estimated[i,1:npar] <- par
        se[i,1:npar] <- sqrt(diag(IM)) # asymptotic S.E.

      } else warning("model is incorrect or unspecified.")
    }
  }
  return(list(item_estimated, se))
}

# An auxiliary function in calculating gradients and Hessian matrices of polytomous items
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

# Mstep_Cont2 <- function(E, item, data){
#   item_estimated <- item
#   item[,3] <- log(item[,3])
#   for(i in 1:nrow(item)){
#     item_estimated[i,] <- optim(item[i,], fn = LL_Cont, gr = grad_Cont, theta=E$Xk, data=data[,i], Pk = E$Pk, method = "BFGS")$par
#   }
#   item_estimated[,3] <- exp(item_estimated[,3])
#   return(list(item_estimated,NULL))
# }

Mstep_Cont <- function(E, item, data, model, threshold = 1e-7, max_iter = 20){
  nitem <- nrow(item)
  item_estimated <- item
  se <- matrix(nrow = nrow(item), ncol = ncol(item))
  for(i in 1:nitem){
    par <- item[i,]
    par[3] <- log(par[3])
    iter <- 0
    div <- 3
    if(model == 1){
      repeat{
        iter <- iter + 1

        l1l2 <- cont_L1L2(item = par, Xk = E$Xk, data = data[,i], Pk = E$Pk)
        diff <- (l1l2[[1]]%*%l1l2[[2]])[-1]

        if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
          par[-1] <- par[-1]
        } else{
          if( sum(abs(diff)) > div){
            par[-1] <- par[-1]-diff/2
          } else {
            par[-1] <- par[-1]-diff
            div <- sum(abs(diff))
          }
        }
        if( div <= threshold | iter > max_iter) break
      }
      par[3] <- exp(par[3])
      item_estimated[i,] <- par
      se[i,-1] <- suppressWarnings(sqrt(-diag(l1l2[[2]])))[-1]
    }else if(model == 2){
      repeat{
        iter <- iter + 1

        l1l2 <- cont_L1L2(item = par, Xk = E$Xk, data = data[,i], Pk = E$Pk)
        diff <- l1l2[[1]]%*%l1l2[[2]]

        if(is.infinite(sum(abs(diff)))|is.na(sum(abs(diff)))){
          par <- par
        } else{
          if( sum(abs(diff)) > div){
            if((max(abs(diff[-1]))/abs(diff[1])>1000) & (abs(par[1]) >= abs(par[1]-diff[1]))){
              par[1] <- -par[1]
            } else{
              par <- par-diff/2
            }
          } else {
            par <- par-diff
            div <- sum(abs(diff))
          }
        }
        if( div <= threshold | iter > max_iter) break
      }
      par[3] <- exp(par[3])
      item_estimated[i,] <- par
      se[i,] <- suppressWarnings(sqrt(-diag(l1l2[[2]])))
    }
  }
  return(list(item_estimated, se))
}


#' @importFrom stats dbeta
#'
LL_Cont <- function(item, theta, data, Pk){
  nu <- exp(item[3])
  mu <- P(theta = rep(theta, each=nrow(Pk)), a = item[1], b = item[2])
  alpha <- mu*nu
  beta <- nu*(1-mu)
  LL <- sum(as.vector(Pk)*log(dbeta(rep(data, times=ncol(Pk)), alpha, beta)), na.rm = TRUE)
  return(-LL)
}


grad_Cont <- function(item, theta, data, Pk){
  nu <- exp(item[3])
  mu <- P(theta = rep(theta, each=nrow(Pk)), a = item[1], b = item[2])
  alpha <- mu*nu
  beta <- nu*(1-mu)
  v1 <- log(rep(data, times=ncol(Pk)))-digamma(alpha)
  v2 <- log(1-rep(data, times=ncol(Pk)))-digamma(beta)
  La <- sum(as.vector(Pk)*(rep(theta, each=nrow(Pk))-item[2])*nu*mu*(1-mu)*(v1-v2), na.rm = TRUE)
  Lb <- sum(-as.vector(Pk)*item[1]*nu*mu*(1-mu)*(v1-v2), na.rm = TRUE)
  Lnu <- sum(as.vector(Pk)*(mu*v1+(1-mu)*v2+digamma(nu)), na.rm = TRUE)
  return(-c(La, Lb, Lnu))
}

cont_L1L2 <- function(item, Xk, data, Pk){
  fk <- colSums(Pk[!is.na(data),])
  nu <- exp(item[3])
  mu <- P(theta = Xk, a = item[1], b = item[2])
  aph <- mu*nu
  bt <- nu*(1-mu)
  s1 <- as.vector(crossprod(log(data[!is.na(data)]), Pk[!is.na(data),]))
  s2 <- as.vector(crossprod(log(1-data[!is.na(data)]), Pk[!is.na(data),]))
  La <- sum(
    (Xk-item[2])*aph*bt/nu*(s1-s2-fk*(digamma(aph)-digamma(bt))),
    na.rm = TRUE)
  Lb <- -item[1]*sum(
    aph*bt/nu*(s1-s2-fk*(digamma(aph)-digamma(bt))),
    na.rm = TRUE)
  Lxi <- nu*sum(
    fk*digamma(nu)+mu*(s1-fk*digamma(aph))+(1-mu)*(s2-fk*digamma(bt)),
    na.rm = TRUE)

  Laa <- -sum(
    (((Xk-item[2])*aph*bt/nu)^2)*fk*(trigamma(aph)+trigamma(bt)),
    na.rm = TRUE
  )
  Lab <- item[1]*sum(
    (Xk-item[2])*((aph*bt/nu)^2)*fk*(trigamma(aph)+trigamma(bt)),
    na.rm = TRUE
  )
  Lbb <- -(item[1]^2)*sum(
    ((aph*bt/nu)^2)*fk*(trigamma(aph)+trigamma(bt)),
    na.rm = TRUE
  )
  Laxi <- -sum(
    (Xk-item[2])*aph*bt*fk*(mu*trigamma(aph)-(1-mu)*trigamma(bt)),
    na.rm = TRUE
  )
  Lbxi <- item[1]*sum(
    aph*bt*fk*(mu*trigamma(aph)-(1-mu)*trigamma(bt)),
    na.rm = TRUE
  )
  Lxixi <- (nu^2)*sum(fk)*trigamma(nu) - sum(
    fk*((aph^2)*trigamma(aph)+(bt^2)*trigamma(bt)),
    na.rm = TRUE)

  LL_matrix <- matrix(c(
    Laa, Lab, Laxi,
    Lab, Lbb, Lbxi,
    Laxi, Lbxi, Lxixi
  ),
  nrow = 3,
  byrow = TRUE)

  return(list(L1 = c(La, Lb, Lxi),
              L2 = solve(LL_matrix))
         )
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
  return(
    list(prob=prob,
         d_raw=d_raw,
         d=d,
         sd_ratio=sd_ratio,
         m=prob*m1+(1-prob)*m2,
         s=s2total
         )
    )
}

#################################################################################################################
# Ability parameter MLE
#################################################################################################################
MLE_theta <- function(item, data, type){
  message("\n",appendLF=FALSE)
  mle <- NULL
  se <- NULL
  if(all(type=="dich")){
    for(i in 1:nrow(data)){
      message("\r","\r","MLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)
      if(sum(data[i,],na.rm = TRUE)==sum(!is.na(data[i,]))){
        mle <- append(mle, Inf)
        se <- append(se, NA)
      } else if(sum(data[i,],na.rm = TRUE)==0){
        mle <- append(mle, -Inf)
        se <- append(se, NA)
      } else {
        th <- 0
        thres <- 1
        iter <- 0
        while((thres > 0.0001) & (iter < 100)){
          iter <- iter + 1
          p_ <- P(theta = th, a = item[,1], b = item[,2], c = 0)
          p <- p_*(1-item[,3])+item[,3]
          L1 <- sum(
            item[,1]*p_/p*(data[i,]-p),
            na.rm = TRUE
          )
          L2 <- -sum(
            item[!is.na(data[i,]),1]^2*p_^2*(1-p)/p
          )
          diff <- L1/L2
          if(abs(diff)>thres){
            th <- th - diff/2
          } else{
            th <- th - diff
            thres <- abs(diff)
          }
        }
        mle <- append(mle, th)
        se <- append(se, sqrt(-1/L2))
      }
    }
  } else if(all(type %in% c("PCM", "GPCM", "GRM"))){
    ncat <- rowSums(!is.na(item))-1
    for(i in 1:nrow(data)){
      message("\r","\r","MLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)
      if(sum(data[i,],na.rm = TRUE)==sum(ncat[!is.na(data[i,])])){
        mle <- append(mle, Inf)
        se <- append(se, NA)
      } else if(sum(data[i,],na.rm = TRUE)==0){
        mle <- append(mle, -Inf)
        se <- append(se, NA)
      } else {
        th <- 0
        thres <- 1
        iter <- 0
        while((thres > 0.0001) & (iter < 100)){
          iter <- iter + 1
          l1l2 <- L1L2_Poly(th, item, data, type, ncat,i )
          diff <- l1l2[1]/l1l2[2]
          if(abs(diff)>thres){
            th <- th - diff/2
          } else{
            th <- th - diff
            thres <- abs(diff)
          }
        }
        mle <- append(mle, th)
        se <- append(se, sqrt(-1/l1l2[2]))
      }
    }
  } else if(any(type %in% c("mix"))){
    ncat <- rowSums(!is.na(item[[2]]))-1
    for(i in 1:nrow(data[[1]])){
      message("\r","\r","MLE for ability parameter estimation, ", i,"/",nrow(data[[1]]),sep="",appendLF=FALSE)
      if(
        sum(c(data[[1]][i,],data[[2]][i,]),na.rm = TRUE)==sum(c(!is.na(data[[1]][i,]),ncat[!is.na(data[[2]][i,])]))
        ){
        mle <- append(mle, Inf)
        se <- append(se, NA)
      } else if(sum(c(data[[1]][i,],data[[2]][i,]),na.rm = TRUE)==0){
        mle <- append(mle, -Inf)
        se <- append(se, NA)
      } else {
        th <- 0
        thres <- 1
        iter <- 0
        while((thres > 0.0001) & (iter < 100)){
          iter <- iter + 1
          # dichotomous items
          p_ <- P(theta = th, a = item[[1]][,1], b = item[[1]][,2], c = 0)
          p <- p_*(1-item[[1]][,3])+item[[1]][,3]
          L1 <- sum(
            item[[1]][,1]*p_/p*(data[[1]][i,]-p),
            na.rm = TRUE
          )
          L2 <- -sum(
            item[[1]][!is.na(data[[1]][i,]),1]^2*p_^2*(1-p)/p
          )

          # polytomous items
          l1l2 <- L1L2_Poly(th=th, item=item[[2]], data=data[[2]], type=type[2], ncat=ncat,i=i)

          # add them
          diff <- (L1+l1l2[1])/(L2+l1l2[2])
          if(abs(diff)>thres){
            th <- th - diff/2
          } else{
            th <- th - diff
            thres <- abs(diff)
          }
        }
        mle <- append(mle, th)
        se <- append(se, sqrt(-1/l1l2[2]))
      }
    }
  } else if(all(type=="cont")){
    for(i in 1:nrow(data)){
      message("\r","\r","MLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)

      th <- 0
      thres <- 1
      iter <- 0
      while((thres > 0.0001) & (iter < 100)){
        iter <- iter + 1
        L1 <- L1_Cont(data = data[i,], theta = th, a = item[,1], b = item[,2], nu = item[,3])
        L2 <- -L2_Cont(theta = th, a = item[!is.na(data[i,]),1], b = item[!is.na(data[i,]),2], nu = item[!is.na(data[i,]),3])
        diff <- sum(L1, na.rm = TRUE)/sum(L2, na.rm = TRUE)
        if(abs(diff)>thres){
          th <- th - diff/2
        } else{
          th <- th - diff
          thres <- abs(diff)
        }
      }
      mle <- append(mle, th)
      se <- append(se, sqrt(-1/sum(L2, na.rm = TRUE)))
    }
  }
  return(list(mle=mle,
              se=se))
}

WLE_theta <- function(item, data, type){
  message("\n",appendLF=FALSE)
  mle <- NULL
  se <- NULL
  if(all(type=="dich")){
    for(i in 1:nrow(data)){
      tryCatch(
        {
          message("\r","\r","WLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)

          th <- 0
          thres <- 1
          iter <- 0
          while((thres > 0.0001) & (iter < 100)){
            iter <- iter + 1
            p_ <- P(theta = th, a = item[,1], b = item[,2], c = 0)
            p <- p_*(1-item[,3])+item[,3]
            L1 <- sum(
              item[,1]*p_/p*(data[i,]-p),
              na.rm = TRUE
            )
            L2 <- -sum(
              item[!is.na(data[i,]),1]^2*p_^2*(1-p)/p
            )
            diff <- (L1+wle(th, item[!is.na(data[i,]),], type))/L2
            if(abs(diff)>thres){
              th <- th - diff/2
            } else{
              th <- th - diff
              thres <- abs(diff)
            }
          }
          mle <- append(mle, th)
          se <- append(se, sqrt(-1/L2))
        }, error = function(e){
          message("\n","WLE failed to converge for the entry ", i,"\n",sep="",appendLF=FALSE)

          mle <<- append(mle, NA)
          se <<- append(se, NA)
        }
      )

    }
  } else if(all(type %in% c("PCM", "GPCM", "GRM"))){
    ncat <- rowSums(!is.na(item))-1
    for(i in 1:nrow(data)){
      tryCatch(
        {
          message("\r","\r","WLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)

          th <- 0
          thres <- 1
          iter <- 0
          while((thres > 0.0001) & (iter < 100)){
            iter <- iter + 1
            l1l2 <- L1L2_Poly(th, item, data, type, ncat,i )
            diff <- (l1l2[1]+wle(th, item[!is.na(data[i,]),], type))/l1l2[2]
            if(abs(diff)>thres){
              th <- th - diff/2
            } else{
              th <- th - diff
              thres <- abs(diff)
            }
          }
          mle <- append(mle, th)
          se <- append(se, sqrt(-1/l1l2[2]))
        }, error = function(e){
          message("\n","WLE failed to converge for the entry ", i,"\n",sep="",appendLF=FALSE)

          mle <<- append(mle, NA)
          se <<- append(se, NA)
        }
      )
    }
  } else if(any(type %in% c("mix"))){
    ncat <- rowSums(!is.na(item[[2]]))-1
    for(i in 1:nrow(data[[1]])){
      tryCatch(
        {
          message("\r","\r","WLE for ability parameter estimation, ", i,"/",nrow(data[[1]]),sep="",appendLF=FALSE)

          th <- 0
          thres <- 1
          iter <- 0
          while((thres > 0.0001) & (iter < 100)){
            iter <- iter + 1
            # dichotomous items
            p_ <- P(theta = th, a = item[[1]][,1], b = item[[1]][,2], c = item[[1]][,3])
            p <- p_*(1-item[[1]][,3])+item[[1]][,3]
            L1 <- sum(
              item[[1]][,1]*p_/p*(data[[1]][i,]-p),
              na.rm = TRUE
            )
            L2 <- -sum(
              item[[1]][!is.na(data[[1]][i,]),1]^2*p_^2*(1-p)/p
            )

            # polytomous items
            l1l2 <- L1L2_Poly(th=th, item=item[[2]], data=data[[2]], type=type[2], ncat=ncat,i=i)

            # add them
            diff <- (L1+l1l2[1]+wle(th, item[[1]][!is.na(data[[1]][i,]),], "dich")+wle(th, item[[2]][!is.na(data[[2]][i,]),], type[2]))/(L2+l1l2[2])
            if(abs(diff)>thres){
              th <- th - diff/2
            } else{
              th <- th - diff
              thres <- abs(diff)
            }
          }
          mle <- append(mle, th)
          se <- append(se, sqrt(-1/l1l2[2]))
        }, error = function(e){
          message("\n","WLE failed to converge for the entry ", i,"\n",sep="",appendLF=FALSE)

          mle <<- append(mle, NA)
          se <<- append(se, NA)
        }
      )

    }
  } else if(all(type=="cont")){
    for(i in 1:nrow(data)){
      tryCatch(
        {
          message("\r","\r","WLE for ability parameter estimation, ", i,"/",nrow(data),sep="",appendLF=FALSE)

          th <- 0
          thres <- 1
          iter <- 0
          while((thres > 0.0001) & (iter < 100)){
            iter <- iter + 1
            L1 <- L1_Cont(data = data[i,], theta = th, a = item[,1], b = item[,2], nu = item[,3])
            L2 <- -L2_Cont(theta = th, a = item[!is.na(data[i,]),1], b = item[!is.na(data[i,]),2], nu = item[!is.na(data[i,]),3])
            diff <- (sum(L1, na.rm = TRUE)+wle(th, item[!is.na(data[i,]),], "cont"))/sum(L2, na.rm = TRUE)
            if(abs(diff)>thres){
              th <- th - diff/2
            } else{
              th <- th - diff
              thres <- abs(diff)
            }
          }
          mle <- append(mle, th)
          se <- append(se, sqrt(-1/sum(L2, na.rm = TRUE)))
        }, error = function(e){
          message("\n","WLE failed to converge for the entry ", i,"\n",sep="",appendLF=FALSE)

          mle <<- append(mle, NA)
          se <<- append(se, NA)
        }
      )

    }
  }
  return(list(mle=mle,
              se=se))
}

L1_Cont <- function(data, theta, a, b, nu=20){
  mu <- P(theta, a, b)
  alpha <- mu*nu
  beta <- nu*(1-mu)
  grad <- (a*alpha*beta/nu)*(-digamma(alpha)+digamma(beta)+log(data/(1-data)))
  return(grad)
}

L2_Cont <- function(theta, a, b, nu=20){
  mu <- P(theta, a, b)
  alpha <- mu*nu
  beta <- nu*(1-mu)
  inform <- (a*alpha*beta/nu)^2*(trigamma(alpha)+trigamma(beta))
  return(inform)
}
# plot(seq(-4,4,length=81), inform_Cont(seq(-4,4,length=81), 1, 0, 5))

L1L2_Poly <- function(th, item, data, type, ncat, i){
  if(type %in% c("PCM", "GPCM")){
    p <- P_P(theta = th, a = item[,1], b = item[,-1])
    p[is.na(p)] <- 0
    S <- p[,-1]%*%1:max(ncat)
    L1 <- sum(
      item[,1]*(data[i,]-S),
      na.rm = TRUE
    )
    L2 <- -sum(
      (item[,1]^2*(p[,-1]%*%(1:max(ncat))^2 - S^2))[!is.na(data[i,])],
      na.rm = TRUE
    )
  } else if(type=='GRM'){
    pmat <- P_G(th, item[,1], item[,-1])
    p_ <- P(th, item[,1], item[,-1])
    ws <- add0(cbind(0, p_*(1-p_), NA))
    q_p <- add0(cbind(0, 1-2*p_, NA))
    L1 <- sum(
      (item[,1]*(ws[,-ncol(ws)]-ws[,-1])/pmat)[cbind(1:length(ncat),data[i,]+1)],
      na.rm = TRUE
    )
    L2 <- sum(
      rowSums(item[,1]^2*((ws*q_p)[,-ncol(ws)]-(ws*q_p)[,-1]-(ws[,-ncol(ws)]-ws[,-1])^2/pmat),
              na.rm = TRUE)[!is.na(data[i,])],
      na.rm = TRUE
    )
  }
  return(c(L1, L2))
}

wle <- function(theta, item, type){
  if(type == "dich"){
    p_ <- P(theta = theta, a = item[,1], b = item[,2], c = 0)
    p0 <- p_*(1-item[,3])+item[,3]
    p1 <- item[,1]*(1-item[,3])*p_*(1-p_)
    p2 <- (item[,1]^2)*(1-item[,3])*p_*(1-p_)*(1-2*p_)
    J <- sum((p1*p2)/(p0*(1-p0)), na.rm = TRUE)
    I <- sum((p1^2)/(p0*(1-p0)), na.rm = TRUE)
  } else if(type %in% c("PCM", "GPCM")){
    p0 <- P_P(theta, a = item[,1], b = item[,-1])
    na_loc <- is.na(p0)
    p0[na_loc] <- 0
    N <- 0:(ncol(p0)-1)
    p1 <- p0 * t(outer(N, p0 %*% N, FUN = "-")[,,1]) * item[,1]
    p2 <- p1 * t(outer(N, p0 %*% N, FUN = "-")[,,1]) * item[,1] - p0 * (c(p1 %*% N) * item[,1])
    p0[na_loc] <- NA
    p1[na_loc] <- NA
    p2[na_loc] <- NA
    J <- sum((p1*p2)/p0, na.rm = TRUE)
    I <- sum((p1^2)/p0, na.rm = TRUE)
  } else if(type == "GRM"){
    p0 <- P_G(theta, item[,1], item[,-1])
    p_ <- P(theta = theta, a = item[,1], b = item[,-1])
    p1_ <- p_*(1-p_)*item[,1]
    p2_ <- p1_*(1-2*p_)*item[,1]
    p1 <- cbind(0, p1_) - add0(cbind(p1_,NA))
    p2 <- cbind(0, p2_) - add0(cbind(p2_,NA))
    J <- sum((p1*p2)/p0, na.rm = TRUE)
    I <- sum((p1^2)/p0, na.rm = TRUE)
  } else if(type == "cont"){
    nu <- item[,3]
    mu <- P(theta, item[,1], item[,2])
    alpha <- mu*nu
    beta <- nu*(1-mu)
    J <- - sum(
      (item[,1]/nu)^3*alpha^2*beta^2*(beta-alpha)*(trigamma(alpha)+trigamma(beta))
             )
    I <- sum(
      ((item[,1]/nu)*alpha*beta)^2*(trigamma(alpha)+trigamma(beta))
    )
  }
  return(J/(2*I))
}

#################################################################################################################
# Linear interpolation / extrapolation
#################################################################################################################
#' @importFrom stats approx
#'
lin_inex <- function(qp, qh, range, rule=2){
  q <- length(qp)
  m <- as.vector(qp%*%qh)
  s <- as.vector((qp-c(m))^2%*%qh)
  qp <- (qp-m)/sqrt(s)
  ap <- approx(qp, y = qh, xout = seq(range[1],range[2],length=q),method = "linear", rule=rule)
  return(
    list(
      qp=ap$x,
      qh=ap$y/sum(ap$y),
      m=m,
      s=s
      )
    )
}

#################################################################################################################
# Latent distribution estimation
#################################################################################################################
latent_dist_est <- function(method, Xk, posterior, range,
                            bandwidth = NULL, par=NULL, N=NULL, q=NULL){
  if(method %in% c("Normal", "normal", "N", "EHM")){
    post_den <- posterior/sum(posterior)
    lin <- lin_inex(Xk, post_den, range = range)
  }
  if(method=='KDE'){
    post_den <- posterior/sum(posterior)
    post_den <- lin_inex(Xk, post_den, range = range)
    nzindex <- round(post_den$qh*N)!=0
    SJPI <- density(rep(Xk[nzindex], times=round(post_den$qh*N)[nzindex]),
                    bw = bandwidth,
                    n=q,
                    from = range[1],
                    to=range[2])
    lin <- lin_inex(Xk, SJPI$y/sum(SJPI$y), range = range)
    lin$m <- post_den$m
    lin$s <- post_den$s
    par <- c(SJPI$bw, SJPI$n)
  }
  if(method %in% c('DC', 'Davidian')){
    post_den <- posterior/sum(posterior)
    post_den <- lin_inex(Xk, post_den, range = range)
    par <- nlminb(start = par,
                  objective = DC.LL,
                  gradient = DC.grad,
                  theta= Xk,
                  freq = post_den$qh*N
                  )$par

    post_den2 <- dcurver::ddc(x = Xk, phi = par)
    post_den2 <- post_den2/sum(post_den2)
    lin <- lin_inex(Xk, post_den2, range = range, rule = 2)
    lin$m <- post_den$m
    lin$s <- post_den$s
  }
  if(method=='LLS'){
    post_den <- posterior/sum(posterior)
    post_den <- lin_inex(Xk, post_den, range = range)
    LLS <- lls(Xk=Xk,
               posterior=post_den$qh*N,
               bb=par,
               N=N
               )
    post_den2 <- LLS$freq/N
    par <- LLS$beta
    lin <- lin_inex(Xk, post_den2, range = range, rule = 2)
    lin$m <- post_den$m
    lin$s <- post_den$s
  }

  return(
    list(
      posterior_density = lin$qh,
      Xk = lin$qp,
      m = lin$m,
      s = lin$s,
      par = par
      )
    )
}

#################################################################################################################
# Gauss-Hermite constants
#################################################################################################################
#' @importFrom usethis use_data
#'
GHc <- c(1,0,
         1,0,
         3,0,
         15,0,
         105,0,
         945,0,
         10395,0,
         135135,0,
         2027025,0,
         34459425,0,
         654729075)

usethis::use_data(GHc, internal = TRUE, overwrite = TRUE)

#################################################################################################################
# B matrix
#################################################################################################################
B_mat <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(sqrt(eigen(mmat)$values))%*%t(eigen(mmat)$vectors)
  return(umat)
}

# B_mat2 <- function(h){
#   mmat <- matrix(nrow = h+1, ncol = h+1)
#   for(i in 1:(1+h)){
#     mmat[i,] <- IRTest::GHc[(1:(1+h))+(i-1)]
#   }
#   umat <- diag(svd(mmat)$d^.25)%*%t(svd(mmat)$v)
#   return(umat)
# }

#################################################################################################################
# c vector
#################################################################################################################
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

#################################################################################################################
# Z vector
#################################################################################################################
# Z_vec <- function(theta, h){
#   theta^(0:h)
# }

#################################################################################################################
# polynomial
#################################################################################################################
# DC_poly <- function(phi, theta, invB, cvec){
#   h <- length(phi)
#   m <- invB%*%cvec
#   dcpoly <- as.vector(t(m)%*%sapply(theta,Z_vec,h=h))
#   return(dcpoly)
# }


#################################################################################################################
# Davidian curve
#################################################################################################################
# dDC <- function(phi, theta){
#   h <- length(phi)
#   invB <- solve(B_mat(h))
#   cvec <- c_vec(phi)
#   densDC <- (DC_poly(phi, theta, invB, cvec))^2*dnormal(theta)
#   return(densDC)
# }

#################################################################################################################
# log-likelihood
#################################################################################################################
# LLDC <- function(phi, theta, freq){
#   -freq%*%log(dDC(phi, theta))
# }

#################################################################################################################
# c-phi matrix
#################################################################################################################
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

#################################################################################################################
# 1st derivative
#################################################################################################################
# grad_DC <- function(phi, theta, freq){
#   h <- length(phi)
#   invB <- solve(B_mat(h))
#   cvec <- c_vec(phi)
#   matalg <- t(c_phi(phi, cvec))%*%t(invB)%*%sapply(c(theta),Z_vec,h=h)
#   dv1 <- -(2*freq/DC_poly(phi, theta, invB, cvec))%*%t(matalg)
#   return(dv1)
# }

#################################################################################################################
# Initial phi
#################################################################################################################
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



#' @importFrom dcurver ddc
#'
DC.LL <- function (phi, theta, freq) {
  LL <- sum(freq * log(dcurver::ddc(theta, phi)))
  -LL
}


#' @importFrom dcurver dc_grad
#'
DC.grad <- function (phi, theta, freq) {
  -freq%*%dcurver::dc_grad(theta, phi)
}

#################################################################################################################
# Uniform to categorical values
#################################################################################################################
yyy <- function(x){
  y <- exp(x)/(1+exp(x))^2
  return(y/sum(y))
}

logistic_means <- function(x){
  cuts <- 1/x*1:(x-1)
  cuts <- c(1e-15, cuts, (1-1e-15))
  cuts <- log(cuts/(1-cuts))

  means <- NULL
  for(i in 1:x){
    xxx <- seq(cuts[i],cuts[i+1],0.0001)
    means <- append(
      means,
      sum(xxx * yyy(xxx))
    )
  }
  return(means)
}

logit_inv <- function(x)exp(x)/(1+exp(x))

unif2cat <- function(data, labels = NULL, x = 5){
  if(is.null(labels)){
    cuts <- 1/x*(1:(x-1))
    cuts <- log(cuts/(1-cuts))
    breaks <- c(-Inf, cuts, Inf)
    labels <- logit_inv(logistic_means(x))
    cut_data <- cut(log(data/(1-data)), breaks = breaks, labels = FALSE)
    return(labels[cut_data])
  } else {
    if(length(data) == 1){
      return(labels[which.min(abs(data - labels))])
    } else {
      cuts <- 1/x*(1:x)
      cuts <- cuts - (cuts[2]-cuts[1])/2
      labss <- apply(abs(outer(data, cuts, FUN = "-")), MARGIN = 1, FUN = which.min)
      return(cuts[labss])
    }
  }
}

