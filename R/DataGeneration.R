#' Generating an artificial item response dataset
#'
#' @description This function generates an artificial item response dataset allowing various options.
#'
#' @importFrom betafunctions rBeta.4P
#' @importFrom stats dnorm rbinom rchisq rnorm runif rbeta
#'
#' @param seed A numeric value that is used for random sampling.
#' Seed number can guarantee a replicability of the result.
#' @param N A numeric value of the number of examinees.
#' @param nitem_D A numeric value of the number of dichotomous items.
#' @param nitem_P A numeric value of the number of polytomous items.
#' @param nitem_C A numeric value of the number of continuous response items.
#' @param model_D A vector or a character string that represents the probability model for the dichotomous items.
#' @param model_P A character string that represents the probability model for the polytomous items.
#' @param latent_dist A character string that determines the type of latent distribution.
#' Currently available options are \code{"beta"} (four-parameter beta distribution; \code{\link{rBeta.4P}}),
#' \code{"chi"} (\eqn{\chi^2} distribution; \code{\link{rchisq}}),
#' \code{"normal"}, \code{"Normal"}, or \code{"N"} (standard normal distribution; \code{\link{rnorm}}),
#' and \code{"Mixture"} or \code{"2NM"} (two-component Gaussian mixture distribution; see Li (2021) for details.)
#' @param item_D An item parameter matrix for using fixed parameter values. The number of columns should be 3: \code{a} parameter for the first, \code{b} parameter for the second, and \code{c} parameter for the third column. Default is \code{NULL}.
#' @param item_P An item parameter matrix for using fixed parameter values. The number of columns should be 7: \code{a} parameter for the first, and \code{b} parameters for the rest of the columns. Default is \code{NULL}.
#' @param item_C An item parameter matrix for using fixed parameter values. The number of columns should be 3: \code{a} parameter for the first, \code{b} parameter for the second, and \code{nu} parameter for the third column. Default is \code{NULL}.
#' @param theta An ability parameter vector for using fixed parameter values. Default is \code{NULL}.
#' @param prob A numeric value for using \code{latent_dist = "2NM"}.
#' It is the \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees belonging to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).
#' @param d A numeric value for using \code{latent_dist = "2NM"}.
#' It is the \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
#' where \eqn{\mu_1} and \eqn{\mu_2} are the estimated means of the first and second Gaussian components, respectively.
#' And \eqn{\bar{\sigma}} is the overall standard deviation of the latent distribution (Li, 2021).
#' Without loss of generality, \eqn{\mu_2 \ge \mu_1} is assumed, thus \eqn{\delta \ge 0}.
#' @param sd_ratio A numeric value for using \code{latent_dist = "2NM"}.
#' It is the \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} and \eqn{\sigma_2} are the estimated standard deviations of the first and second Gaussian components, respectively (Li, 2021).
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
#' @param categ A scalar or a numeric vector of length \code{nitem_P}. The default is 5.
#' If \code{length(categ)>1}, the \emph{i}th element equals the number of categories of the \emph{i}th polyotomous item.
#' @param possible_ans Possible options for continuous items (e.g., 0.1, 0.3, 0.5, 0.7, 0.9)
#'
#' @return This function returns a \code{list} of several objects:
#' \item{theta}{A vector of ability parameters (\eqn{\theta}).}
#' \item{item_D}{A matrix of dichotomous item parameters.}
#' \item{initialitem_D}{A matrix that contains initial item parameter values for dichotomous items.}
#' \item{data_D}{A matrix of dichotomous item responses where rows indicate examinees and columns indicate items.}
#' \item{item_P}{A matrix of polytomous item parameters.}
#' \item{initialitem_P}{A matrix that contains initial item parameter values for polytomous items.}
#' \item{data_P}{A matrix of polytomous item responses where rows indicate examinees and columns indicate items.}
#' \item{item_D}{A matrix of continuous response item parameters.}
#' \item{initialitem_D}{A matrix that contains initial item parameter values for continuous response items.}
#' \item{data_D}{A matrix of continuous response item responses where rows indicate examinees and columns indicate items.}
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
#' # Dichotomous item responses
#'
#' Alldata <- DataGeneration(N = 500,
#'                           nitem_D = 10)
#'
#'
#' # Polytomous item responses
#'
#' Alldata <- DataGeneration(N = 1000,
#'                           nitem_P = 10)
#'
#'
#' # Mixed-format items
#'
#' Alldata <- DataGeneration(N = 1000,
#'                           nitem_D = 20,
#'                           nitem_P = 10)
#'
#' # Continuous items
#'
#' AllData <- DataGeneration(N = 1000,
#'                           nitem_C = 10)
#'
#' # Dataset from non-normal latent density using two-component Gaussian mixture distribution
#'
#' Alldata <- DataGeneration(N=1000,
#'                           nitem_P = 10,
#'                           latent_dist = "2NM",
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
DataGeneration <- function(seed=1, N=2000,
                           nitem_D=0, nitem_P=0, nitem_C=0,
                           model_D="2PL", model_P="GPCM",
                           latent_dist="Normal",
                           item_D=NULL, item_P=NULL, item_C=NULL,
                           theta = NULL,
                           prob=0.5, d=1.7, sd_ratio=1,
                           m = 0, s = 1,
                           a_l=0.8, a_u=2.5,
                           b_m=NULL, b_sd=NULL,
                           c_l=0, c_u=0.2, categ=5,
                           possible_ans = seq(.1,.9,length=5)){
  initialitem_D=NULL; data_D=NULL
  initialitem_P=NULL; data_P=NULL
  initialitem_C=NULL; data_C=NULL

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


  # item parameters for polytomous items
  if(is.null(item_C)){
    a_l=0.5; a_u=1.5
    if((nitem_C!=0)&(!is.null(nitem_C))){
      data_C <- matrix(nrow = N, ncol = nitem_C)
      item_C <- matrix(nrow = nitem_C, ncol = 3)
      initialitem_C <- matrix(nrow = nitem_C, ncol = 3)
      set.seed(seed)
      item_C[,1] <- 1
      initialitem_C[,1] <- 1
      set.seed(seed)
      item_C[,2] <- round(
        sample(
          x = seq(-2*b_sd+b_m,2*b_sd+b_m,by=0.01*b_sd),
          size = nitem_C,
          replace = TRUE,
          prob = dnorm(seq(-2,2,by=0.01))
        ),
        digits = 2)
      initialitem_C[,2] <- 0

      item_C[,1] <- round(
        runif(nitem_C,a_l,a_u),
        digits = 2
      )
      initialitem_C[,1] <- (a_l+a_u)/2

      item_C[,3] <- 10
      initialitem_C[,3] <- 10

      # item responses
      # possible_ans <- seq(0.05,.95,length=10)#c(.1, .3, .5, .7, .9)

      for(i in 1:nitem_C){
        for(j in 1:N){
          p <- P(theta = theta[j], a = item_C[i,1], b = item_C[i,2])
          data_C[j,i] <- possible_ans[which.min(abs(rbeta(1, p*item_C[i,3], (1-p)*item_C[i,3]) - possible_ans))]
        }
      }
    }
  } else {
    if((nitem_C!=0)&(!is.null(nitem_C))){
      data_C <- matrix(nrow = N, ncol = nitem_C)
      initialitem_C <- matrix(nrow = nitem_C, ncol = 3)
      set.seed(seed)
      initialitem_C[,1] <- (a_l+a_u)/2
      initialitem_C[,2] <- 0
      initialitem_C[,3] <- 10

      # item responses
      # possible_ans <- seq(0.05,.95,length=10)#c(.1, .3, .5, .7, .9)

      for(i in 1:nitem_C){
        for(j in 1:N){
          p <- P(theta = theta[j], a = item_C[i,1], b = item_C[i,2])
          data_C[j,i] <- possible_ans[which.min(abs(rbeta(1, p*item_C[i,3], (1-p)*item_C[i,3]) - possible_ans))]
        }
      }
    }
  }

  return(list(theta=theta,
              item_D=item_D, initialitem_D=initialitem_D, data_D=data_D,
              item_P=item_P, initialitem_P=initialitem_P, data_P=data_P,
              item_C=item_C, initialitem_C=initialitem_C, data_C=data_C))
}
