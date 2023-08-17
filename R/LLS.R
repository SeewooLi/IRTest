lls <- function(Xk, posterior, bb, N){
  ff <- posterior
  Z <- NULL
  for(i in 1:length(bb)){
    Z <- cbind(Z,Xk^i)
  }
  thres <- 1*N
  f <- as.vector(exp(Z%*%bb))
  f <- f/sum(f)*N
  while(thres>0.0001*N){
    sigma_f <- diag(as.vector(f))-f%*%t(f)/N
    diff <- solve(t(Z)%*%sigma_f%*%Z)%*%t(Z)%*%(ff-f)

    temp <- as.vector(exp(Z%*%(bb + diff)))
    temp <- temp/sum(temp)*N
    thres <- max(abs(f-temp))

    if(is.nan(thres)|is.na(thres)) stop("Newton's algorithm for LLS failed to converge.")
    f <- temp
    bb <- bb + diff
  }
  return(
    list(
      freq = f,
      beta = bb
      )
  )
}

