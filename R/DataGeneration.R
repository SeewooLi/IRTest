#' Data Generation
#' @description Data Generation for Computer Simulation
#' @param seed seed number
#' @param N Number of observations to be sampled.
#' @param nitem Number of items to be sampled.
#' @param prob ratio of two distributions.
#' @param d the constant.
#' @param sd_ratio ratio of standard deviation.
#' @param a_l lower limit of a(item discrimination).
#' @param a_u upper limit of a(item discrimination).
#' @param latent_dist Type of latent distribution.
#' (beta, normal, chi)
#' @param model parameter logistic model of item response theory.
#' 1 means one parameter logistic model, 2 means two parameter logistic model, 3 means three parameter logistic model.
#' @param c_l lower limit of c(guessing parameter).
#' @param c_u upper limit of c(guessing parameter).
#'
#' @return Data for computer simulation (item, initialitem, theta, data)
#' @export DataGeneration
#' @importFrom betafunctions
#'
#' @examples
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
  }

  return(list(item=item, initialitem=initialitem, theta=theta, data=data))
}
