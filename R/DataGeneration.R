#' Generation of artificial item response data
#'
#' @description This function generates artificial item response data in which users can specify the types of items and latent distribution.
#'
#'
#' @param seed A numeric value that is used on random sampling.
#' The seed number can guarantee the replicability of the result.
#' @param N a
#' @param nitem_D a
#' @param nitem_P a
#' @param prob a
#' @param d a
#' @param sd_ratio a
#' @param a_l a
#' @param a_u a
#' @param latent_dist a
#' @param model_D a
#' @param model_P a
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
DataGeneration <- function(seed=1, N=2000, nitem_D=NULL, nitem_P=NULL, prob=0.5, d=1.7,
                           sd_ratio=1, a_l=0.8, a_u=2.5, latent_dist="normal",
                           model_D, model_P="GPCM", c_l=0, c_u=0.2, categ){

  # ability parameters (i.e., theta)

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
  } else stop("Specify the type of the latent distribution.")



  # item parameters for dichotomous items

  if(nitem_D!=0){
    data_D <- matrix(nrow = N, ncol = nitem_D)
    item_D <- matrix(nrow = nitem_D, ncol = 3)
    initialitem_D <- matrix(nrow = nitem_D, ncol = 3)
    set.seed(seed)
    for(i in 1:nitem_D){
      if(model_D[i]==1){
        item_D[i,] <- round(c(1,
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              0), digits = 2)
        initialitem_D[i,] <- c(1,0,0)
      }else if(model_D[i]==2){
        item_D[i,] <- round(c(runif(1,a_l,a_u),
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              0), digits = 2)
        initialitem_D[i,] <- c((a_l+a_u)/2,0,0)
      } else if(model_D[i]==3){
        item_D[i,] <- round(c(runif(1,a_l,a_u),
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              runif(1, min = c_l, max = c_u)), digits = 2)
        initialitem_D[i,] <- c((a_l+a_u)/2,0,0)
      }

      # item responses for dichotomous items

      for(j in 1:N){
        p <- P(theta = theta[j], a = item_D[i,1], b = item_D[i,2], c= item_D[i,3])
        data_D[j,i] <- rbinom(1,1,prob = p)
      }
    }
  }


  # item parameters for polytomous items

  if(nitem_P!=0){
    data_P <- matrix(nrow = N, ncol = nitem_P)
    item_P <- matrix(nrow = nitem_P, ncol = 7)
    initialitem_P <- matrix(nrow = nitem_P, ncol = 7)
    set.seed(seed)
    for(i in 1:nitem_P){
      if(model_P=="GPCM"){
        item_P[i,1] <- round(runif(1,a_l,a_u), digits = 2)
        center <- rnorm(1,0,.5)
        item_P[i,2:(categ[i])] <- sort(rnorm(categ[i]-1,center,.2))

        initialitem_P[i,1] <- 1
        initialitem_P[i,2:(categ[i])] <- 0#(-2:1+.5)/3
        }


      # item responses for polytomous items

      for(j in 1:N){
        pp <- P_P(theta = theta[j], a = item_P[i,1], b = item_P[i,-1])
        pp <- pp[!is.na(pp)]
        data_P[j,i] <- sample(x = 0:(categ[i]-1),1,prob=pp)
      }
    }
  }
  return(list(theta=theta,
              item_D=item_D, initialitem_D=initialitem_D, data_D=data_D,
              item_P=item_P, initialitem_P=initialitem_P, data_P=data_P))
}
