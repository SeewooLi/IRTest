#' M1step
#'
#' @return
#' @export
#'
#' @examples
#'
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
