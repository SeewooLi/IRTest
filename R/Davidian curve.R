#################################################################################################################
# Gauss-Hermite constants
#################################################################################################################
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
B_mat2 <- function(h){
  mmat <- matrix(nrow = h+1, ncol = h+1)
  for(i in 1:(1+h)){
    mmat[i,] <- GHc[(1:(1+h))+(i-1)]
  }
  umat <- diag(svd(mmat)$d^.25)%*%t(svd(mmat)$v)
  return(umat)
}

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
Z_vec <- function(theta, h){
  theta^(0:h)
}

#################################################################################################################
# polynomial
#################################################################################################################
DC_poly <- function(phi, theta, invB, cvec){
  h <- length(phi)
  m <- invB%*%cvec
  dcpoly <- as.vector(t(m)%*%sapply(theta,Z_vec,h=h))
  return(dcpoly)
}


#################################################################################################################
# Davidian curve
#################################################################################################################
dDC <- function(phi, theta){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  densDC <- (DC_poly(phi, theta, invB, cvec))^2*dnormal(theta)
  return(densDC)
}
#plot(seq(-6,6,.1),dcurver::ddc(phi = ppp, seq(-6,6,.1)),xlim = c(-4,4),ylim=c(0,0.6))

#################################################################################################################
# log-likelihood
#################################################################################################################
LLDC <- function(phi, theta, freq){
  -freq%*%log(dDC(phi, theta))
}

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
grad_DC <- function(phi, theta, freq){
  h <- length(phi)
  invB <- solve(B_mat(h))
  cvec <- c_vec(phi)
  matalg <- t(c_phi(phi, cvec))%*%t(invB)%*%sapply(c(theta),Z_vec,h=h)
  dv1 <- -(2*freq/DC_poly(phi, theta, invB, cvec))%*%t(matalg)
  return(dv1)
} 

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



DC.LL <- function (phi, theta, freq) {
  LL <- sum(freq * log(dcurver::ddc(theta, phi)))
  -LL
}
DC.grad <- function (phi, theta, freq) {
  -colSums(dcurver::dc_grad(theta, phi) * freq)
}


#ppp<-nlminb(start = rep(1,9), 
#                    objective = optim_phi, 
#                    gradient = optim_phi_grad,
#                    hp=9,
#                    lower = -pi/2, 
#                    upper = pi/2)$par
#solve(B_mat(5))%*%c_vec(ppp)
#plot(seq(-6,6,0.1),dDC(
#  phi = nlminb(start = c(.1,.1,.1), 
#               objective = LLDC, 
#               gradient = deriv1_DC, 
#               lower = -pi/2, 
#               upper = pi/2, 
#               theta=seq(-6,6,0.1), 
#               freq = Kres$fk)$par,
#  theta = seq(-6,6,0.1)))




