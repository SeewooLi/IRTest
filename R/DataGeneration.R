#' Generation of artificial item response data
#'
#' @description This function generates artificial item response data in which users can specify the types of items and latent distribution.
#'
#'
#' @param seed A numeric value that is used on random sampling.
#' The seed number can guarantee the replicability of the result.
#' @param N a
#' @param nitem a
#' @param prob a
#' @param d a
#' @param sd_ratio a
#' @param a_l a
#' @param a_u a
#' @param latent_dist a
#' @param model a
#' @param c_l a
#' @param c_u a
#' @param categ a
#'
#' @return
#' @export
#'
#' @importFrom betafunctions rBeta.4P
#' @importFrom stats dnorm rbinom rchisq rnorm runif
#'
#' @examples
DataGeneration <- function(seed=1, N=2000, nitem=10, prob=0.5, d=1.7,
                           sd_ratio=1, a_l=0.8, a_u=2.5, latent_dist="normal",
                           model=3, c_l=0, c_u=0.2, categ){

  if(model==1){
    set.seed(seed)
    item <- matrix(c(rep(1,nitem),
                     sample(seq(-2,2,by=0.01),nitem, prob = dnorm(seq(-2,2,by=0.01)))), ncol=2)
    item <- round(item, digits = 2)
    initialitem <- matrix(c(rep((a_l+a_u)/2,nitem),
                            rep(0,2*nitem)), ncol=3)
  }else if(model==2){
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
      item[i,2:(categ[i])] <- sort(rnorm(categ[i]-1,center,.2))
    }
    initialitem <- matrix(nrow = nitem, ncol = 7)
    initialitem[,1] <- 1
    for(i in 1:nitem){
      initialitem[i,2:(categ[i])] <- 0#(-2:1+.5)/3
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

  if(model==1|model==2){
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
