#' Generating artificial item response data
#'
#' @description This function generates artificial item response data with users specified item types, details of item parameters, and latent distribution.
#'
#' @importFrom betafunctions rBeta.4P
#' @importFrom stats dnorm rbinom rchisq rnorm runif
#'
#' @param seed A numeric value that is used on random sampling.
#' The seed number can guarantee the replicability of the result.
#' @param N A numeric value. The number of examinees.
#' @param nitem_D A numeric value. The number of dichotomous items.
#' @param nitem_P A numeric value. The number of polytomous items.
#' @param model_D A vector of length \code{nitem_D}.
#' The \emph{i}th element is the probability model for the \emph{i}th dichotomous item.
#' @param model_P A character string that represents the probability model for the polytomous items.
#' @param latent_dist A character string that determines the type of latent distribution.
#' Currently available options are \code{"beta"} (four-parameter beta distribution; \code{\link{rBeta.4P}}),
#' \code{"chi"} (\eqn{\chi^2} distribution; \code{\link{rchisq}}),
#' \code{"normal"}, \code{"Normal"}, or \code{"N"} (standard normal distribution; \code{\link{rnorm}}),
#' and \code{"Mixture"} or \code{"2NM"} (two-component Gaussian mixture distribution; see Li (2021) for details.)
#' @param item_D Default is \code{NULL}. An item parameter matrix can be specified. The number of columns should be 3: \code{a} parameter for the first, \code{b} parameter for the second, and \code{c} parameter for the third column.
#' @param item_P Default is \code{NULL}. An item parameter matrix can be specified. The number of columns should be 7: \code{a} parameter for the first, and \code{b} parameters for the rest of the columns.
#' @param theta Default is NULL. An ability parameter vector can be specified.
#' @param prob A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees who belong to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).
#' @param d A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
#' where \eqn{\mu_1} is the estimated mean of the first Gaussian component,
#' \eqn{\mu_2} is the estimated mean of the second Gaussian component,
#' and \eqn{\bar{\sigma} = 1} is the standard deviation of the latent distribution (Li, 2021).
#' Without loss of generality, \eqn{\mu_2 \ge \mu_1}, thus \eqn{\delta \ge 0}, is assumed.
#' @param sd_ratio A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} is the estimated standard deviation of the first Gaussian component, \eqn{\sigma_2} is the estimated standard deviation of the second Gaussian component (Li, 2021).
#' @param m A numeric value of the overall mean of the latent distribution. The default is 0.
#' @param s A numeric value of the overall standard deviation of the latent distribution. The default is 1.
#' @param a_l A numeric value. The lower bound of item discrimination parameters (\emph{a}).
#' @param a_u A numeric value. The upper bound of item discrimination parameters (\emph{a}).
#' @param b_m A numeric value. The mean of item difficulty parameters (\emph{b}).
#' If unspecified, \code{m} is passed on to the value.
#' @param b_sd A numeric value. The standard deviation of item difficulty parameters (\emph{b}).
#' If unspecified, \code{s} is passed on to the value.
#' @param c_l A numeric value. The lower bound of item guessing parameters (\emph{c}).
#' @param c_u A numeric value. The lower bound of item guessing parameters (\emph{c}).
#' @param categ A numeric vector of length \code{nitem_P}.
#' The \emph{i}th element equals the number of categories of the \emph{i}th polyotomous item.
#'
#' @return This function returns a \code{list} which contains several objects:
#' \item{theta}{A vector of ability parameters (\eqn{\theta}).}
#' \item{item_D}{A matrix of dichotomous item parameters.}
#' \item{initialitem_D}{A matrix that contains initial item parameter values for dichotomous items.}
#' \item{data_D}{A matrix of dichotomous item responses where rows indicate examinees and columns indicate items.}
#' \item{item_P}{A matrix of polytomous item parameters.}
#' \item{initialitem_P}{A matrix that contains initial item parameter values for polytomous items.}
#' \item{data_P}{A matrix of polytomous item responses where rows indicate examinees and columns indicate items.}
#'
#' @export
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
#'
#'
#' @examples
#' # Dichotomous item responses only
#'
#' Alldata <- DataGeneration(seed = 1,
#'                           model_D = rep(3, 10),
#'                           N=500,
#'                           nitem_D = 10,
#'                           nitem_P = 0,
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_D
#' item <- Alldata$item_D
#' initialitem <- Alldata$initialitem_D
#' theta <- Alldata$theta
#'
#'
#' # Polytomous item responses only
#'
#' Alldata <- DataGeneration(seed = 2,
#'                           N=1000,
#'                           item_D=NULL,
#'                           item_P=NULL,
#'                           theta = NULL,
#'                           nitem_D = 0,
#'                           nitem_P = 10,
#'                           categ = rep(3:7,each = 2),
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_P
#' item <- Alldata$item_P
#' initialitem <- Alldata$initialitem_P
#' theta <- Alldata$theta
#'
#'
#' # Mixed-format items
#'
#' Alldata <- DataGeneration(seed = 2,
#'                           model_D = rep(1:2, each=10),# 1PL model is applied to item #1~10
#'                                                       # and 2PL model is applied to item #11~20.
#'                           N=1000,
#'                           nitem_D = 20,
#'                           nitem_P = 10,
#'                           categ = rep(3:7,each = 2),# 3 categories for item #21-22,
#'                                                     # 4 categories for item #23-24,
#'                                                     # ...,
#'                                                     # and 7 categories for item #29-30.
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' DataD <- Alldata$data_D
#' DataP <- Alldata$data_P
#' itemD <- Alldata$item_D
#' itemP <- Alldata$item_P
#' initialitemD <- Alldata$initialitem_D
#' initialitemP <- Alldata$initialitem_P
#' theta <- Alldata$theta
#'
DataGeneration <- function(seed=1, N=2000,
                           nitem_D=0, nitem_P=0,
                           model_D="2PL", model_P="GPCM",
                           latent_dist=NULL,
                           item_D=NULL, item_P=NULL,
                           theta = NULL,
                           prob=0.5, d=1.7, sd_ratio=1,
                           m = 0, s = 1,
                           a_l=0.8, a_u=2.5,
                           b_m=NULL, b_sd=NULL,
                           c_l=0, c_u=0.2, categ=NULL){
  initialitem_D=NULL; data_D=NULL
  initialitem_P=NULL; data_P=NULL

  if(!is.null(categ)&length(categ)==1){
    categ <- rep(categ, nitem_P)
  }
  if(length(model_D)==1){
    model_D <- rep(model_D, nitem_D)
  }
  if(is.null(b_m)){
    b_m <- m
  }
  if(is.null(b_sd)){
    b_sd <- s
  }

  # ability parameters (i.e., theta)
  if(is.null(theta)){
    if(is.null(latent_dist)){
      stop("Specify the type of the latent distribution.")
    }else if(latent_dist=="beta"){
      set.seed(seed)
      theta <- rBeta.4P(n=N, alpha = 3.79, beta= 10.21, l=-2.36, u=6.36)
    }else if(latent_dist=="chi"){
      set.seed(seed)
      theta <- scale(rchisq(N,df=8))
    }else if(latent_dist%in%c("Mixture", "2NM")){
      n1 <- round(N*prob)
      n2 <- N-n1
      m1 <- m-(1-prob)*d*s
      m2 <- m+prob*d*s
      s1 <- sqrt((1-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))*s
      s2 <- s1*sd_ratio

      set.seed(seed)
      theta <- c(rnorm(n=n1,mean=m1,sd=s1),rnorm(n=n2,mean=m2,sd=s2))
    }else if(latent_dist %in% c("normal", "Normal", "N")){
      set.seed(seed)
      theta <- rnorm(n=N,mean=0,sd=1)
    } else stop("Specify the type of the latent distribution.")
  }


  # item parameters for dichotomous items
  if(is.null(item_D)){
    if((nitem_D!=0)&(!is.null(nitem_D))){
      data_D <- matrix(nrow = N, ncol = nitem_D)
      item_D <- matrix(0, nrow = nitem_D, ncol = 3)
      item_D[,1] <- 1
      initialitem_D <- matrix(0, nrow = nitem_D, ncol = 3)
      initialitem_D[,1] <- 1
      set.seed(seed)
      item_D[,2] <- round(
        sample(
          x = seq(-2*b_sd+b_m,2*b_sd+b_m,by=0.01*b_sd),
          size = nitem_D,
          replace = TRUE,
          prob = dnorm(seq(-2,2,by=0.01))
          ),
        digits = 2)


      for(i in 1:nitem_D){
        if(model_D[i] %in% c(2,"2PL",3,"3PL")){
          item_D[i,1] <- round(
            runif(1,a_l,a_u),
            digits = 2
          )
          initialitem_D[i,1] <- (a_l+a_u)/2
        }
        if(model_D[i] %in% c(3,"3PL")){
          item_D[i,3] <- round(
            runif(1, min = c_l, max = c_u),
            digits = 2)
        }

        # item responses for dichotomous items
        for(j in 1:N){
          p <- P(theta = theta[j], a = item_D[i,1], b = item_D[i,2], c= item_D[i,3])
          data_D[j,i] <- rbinom(1,1,prob = p)
        }
      }
    }
  } else {
    if((nitem_D!=0)&(!is.null(nitem_D))){
      data_D <- matrix(nrow = N, ncol = nitem_D)
      initialitem_D <- matrix(nrow = nitem_D, ncol = 3)

      for(i in 1:nitem_D){
        if(model_D[i] %in% c(1, "1PL", "Rasch")){
          initialitem_D[i,] <- c(1,0,0)
        } else {
          initialitem_D[i,] <- c((a_l+a_u)/2,0,0)
        }

        # item responses for dichotomous items

        for(j in 1:N){
          p <- P(theta = theta[j], a = item_D[i,1], b = item_D[i,2], c= item_D[i,3])
          data_D[j,i] <- rbinom(1,1,prob = p)
        }
      }
    }
  }




  # item parameters for polytomous items
  if(is.null(item_P) & !is.null(categ)){
    if((nitem_P!=0)&(!is.null(nitem_P))){
      data_P <- matrix(nrow = N, ncol = nitem_P)
      item_P <- matrix(nrow = nitem_P, ncol = max(categ))
      initialitem_P <- matrix(nrow = nitem_P, ncol = max(categ))
      set.seed(seed)
      for(i in 1:nitem_P){
        center <- rnorm(1,b_m,b_sd*.5)
        if(model_P=="PCM"){
          item_P[i,1] <- 1
          item_P[i,2:(categ[i])] <- sort(rnorm(categ[i]-1,center,1))
        } else if(model_P %in% c("GPCM", "GRM")){
          item_P[i,1] <- round(runif(1,a_l,a_u), digits = 2)
          item_P[i,2:(categ[i])] <- sort(rnorm(categ[i]-1,center,1))
        }
        initialitem_P[i,1] <- 1
        initialitem_P[i,2:(categ[i])] <- seq(-.5,.5,length.out=categ[i]-1)

        # item responses for polytomous items

        for(j in 1:N){
          if(model_P %in% c("GPCM", "PCM")){
            pp <- P_P(theta = theta[j], a = item_P[i,1], b = item_P[i,-1])
          } else if(model_P %in% c("GRM")){
            pp <- P_G(theta = theta[j], a = item_P[i,1], b = item_P[i,-1])
          }
          pp <- pp[!is.na(pp)]
          data_P[j,i] <- sample(x = 0:(categ[i]-1),1,prob=pp)
        }
      }
    }
  } else {
    if((nitem_P!=0)&(!is.null(nitem_P))){
      data_P <- matrix(nrow = N, ncol = nitem_P)
      initialitem_P <- matrix(nrow = nitem_P, ncol = 7)
      set.seed(seed)
      for(i in 1:nitem_P){
        if(model_P %in% c("PCM", "GPCM")){
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
  }
  return(list(theta=theta,
              item_D=item_D, initialitem_D=initialitem_D, data_D=data_D,
              item_P=item_P, initialitem_P=initialitem_P, data_P=data_P))
}
