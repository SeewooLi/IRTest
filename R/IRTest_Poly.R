#' Item and ability parameters estimation for polytomous items
#'
#' @description This function estimates IRT item and ability parameters when all items are scored polytomously.
#' Based on Bock & Aitkin's (1981) marginal maximum likelihood and EM algorithm (EM-MML), this function incorporates several latent distribution estimation algorithms which could free the normality assumption on the latent variable.
#' Reflecting some features of the unknown true latent distribution, application of this latent distribution estimation method could provide more accurate parameter estimates when the normality assumption is violated (Li, 2021; Woods & Lin, 2009; Woods & Thissen, 2006).
#' Only generalized partial credit model (GPCM) is currently available.
#'
#' @importFrom stats density nlminb
#' @importFrom utils flush.console
#'
#' @param initialitem A matrix of initial item parameter values for starting the estimation algorithm
#' @param data A matrix of item responses where responses are coded as 0 or 1.
#' Rows and columns indicate examinees and items, respectively.
#' @param range Range of the latent variable to be considered in the quadrature scheme.
#' The default is from \code{-6} to \code{6}: \code{c(-6, 6)}.
#' @param q A numeric value that represents the number of quadrature points. The default value is 121.
#' @param model A vector that represents types of item characteristic functions applied to each item.
#' However, only generalized partial credit model (\code{GPCM}) is currently available.
#' and \code{3}, \code{"3PL"} for three-parameter logistic model.
#' @param latent_dist A character string that determines latent distribution estimation method.
#' Insert \code{"Normal"}, \code{"normal"}, or \code{"N"} to assume normal distribution on the latent distribution,
#' \code{"EHM"} for empirical histogram method (Mislevy, 1984; Mislevy & Bock, 1985),
#' \code{"Mixture"} for the method that uses two-component Gaussian mixture distribution (Li, 2021; Mislevy, 1984),
#' \code{"DC"} for Davidian-curve method (Woods & Lin, 2009),
#' and \code{"KDE"} for kernel density estimation method (Li, 2022).
#' The default value is set to \code{"Normal"} to follow the conventional assumption on latent distribution.
#'
#' @param max_iter A numeric value that determines the maximum number of iterations in the EM-MML.
#' The default value is 200.
#' @param threshold A numeric value that determines the threshold of EM-MML convergence.
#' A maximum item parameter change is monitored and compared with the threshold.
#' The default value is 0.0001.
#' @param bandwidth A character value is needed when \code{"KDE"} is used for the latent distribution estimation.
#' This argument determines which bandwidth estimation method is used for \code{"KDE"}.
#' The default value is \code{"SJ-ste"}. See \code{\link{density}} for possible options.
#' @param h A natural number less than or equal to 10 is needed when \code{"DC"} is used for the latent distribution estimation.
#' This argument determines the complexity of Davidian curve.
#'
#' @details
#' \describe{
#' \item{
#' The probability for scoring \eqn{k} (i.e., \eqn{u=k; k=0, 1, ..., m; m \ge 2}) in generalized partial credit model (GPCM) can be expressed as follows;
#' }{
#' 1) generalized partial credit model (GPCM)
#' \deqn{P(u=0|\theta, a, b_1, ..., b_{m})=\frac{1}{1+\sum_{c=1}^{m}{\exp{\left[\sum_{v=1}^{c}{a(\theta-b_v)}\right]}}}}
#' \deqn{P(u=1|\theta, a, b_1, ..., b_{m})=\frac{\exp{(a(\theta-b_1))}}{1+\sum_{c=1}^{m}{\exp{\left[\sum_{v=1}^{c}{a(\theta-b_v)}\right]}}}}
#' \deqn{\vdots}
#' \deqn{P(u=m|\theta, a, b_1, ..., b_{m})=\frac{\exp{\left[\sum_{v=1}^{m}{a(\theta-b_v)}\right]}}{1+\sum_{c=1}^{m}{\exp{\left[\sum_{v=1}^{c}{a(\theta-b_v)}\right]}}}}
#'
#' }
#'
#' \item{
#' The estimated latent distribution for each of the latent distribution estimation method can be expressed as follows;
#' }{
#' 1) Empirical histogram method
#' \deqn{P(\theta=X_k)=A(X_k)}
#' where \eqn{k=1, 2, ..., q}, \eqn{X_k} is the location of the \eqn{k}th quadrature point, and \eqn{A(X_k)} is a value of probability mass function evaluated at \eqn{X_k}.
#' Empirical histogram method thus has \eqn{q-1} parameters.
#'
#' 2) Two-component Gaussian mixture distribution
#' \deqn{P(\theta=X)=\pi \phi(X; \mu_1, \sigma_1)+(1-\pi) \phi(X; \mu_2, \sigma_2)}
#' where \eqn{\phi(X; \mu, \sigma)} is the value of a Gaussian component with mean \eqn{\mu} and standard deviation \eqn{\sigma} evaluated at \eqn{X}.
#'
#' 3) Davidian curve method
#' \deqn{P(\theta=X)=\left\{\sum_{\lambda=0}^{h}{{m}_{\lambda}{\theta}^{\lambda}}\right\}^{2}\phi(X; 0, 1)}
#' where \eqn{h} is the degree of the polynomial and corresponds to the argument \code{h}.
#'
#' 4) Kernel density estimation method
#' \deqn{P(\theta=X)=\frac{1}{Nh}\sum_{j=1}^{N}{K\left(\frac{X-\theta_j}{h}\right)}}
#' where \eqn{N} is the number of examinees, \eqn{\theta_j} is \eqn{j}th examinee's ability parameter, \eqn{h} is the bandwidth which corresponds to the argument \code{bw}, and \eqn{K( \bullet )} is a kernel function.
#' The Gaussian kernel is used in this function.
#'
#' }
#' }
#'
#' @return This function returns a \code{list} which contains several objects:
#' \item{par_est}{The item parameter estimates.}
#' \item{se}{The standard errors for item parameter estimates.}
#' \item{fk}{The estimated frequencies of examinees at each quadrature points.}
#' \item{iter}{The number of EM-MML iterations required for the convergence.}
#' \item{prob}{The estimated \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees who belong to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).}
#' \item{d}{The estimated \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
#' where \eqn{\mu_1} is the estimated mean of the first Gaussian component,
#' \eqn{\mu_2} is the estimated mean of the second Gaussian component,
#' and \eqn{\bar{\sigma} = 1} is the standard deviation of the latent distribution (Li, 2021).
#' Without loss of generality, \eqn{\mu_2 \ge \mu_1}, thus \eqn{\delta \ge 0}, is assumed.}
#' \item{sd_ratio}{The estimated \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} is the estimated standard deviation of the first Gaussian component, \eqn{\sigma_2} is the estimated standard deviation of the second Gaussian component (Li, 2021).}
#' \item{quad}{The location of quadrature points.}
#' \item{diff}{The final value of the monitored maximum item parameter change.}
#' \item{Ak}{The estimated discrete latent distribution.
#' It is discrete (i.e., probability mass function) since quadrature scheme of EM-MML is used.}
#' \item{Pk}{The posterior probabilities for each examinees at each quadrature points.}
#' \item{theta}{The estimated ability parameter values.
#' Expected \emph{a posteriori} (EAP) is used for ability parameter estimation.}
#' \item{logL}{The deviance (i.e., -2\emph{log}L).}
#' \item{bw}{The bandwidth used.}
#' \item{Options}{A replication of input arguments.}
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Bock, R. D., & Aitkin, M. (1981). Marginal maximum likelihood estimation of item parameters: Application of an EM algorithm. \emph{Psychometrika, 46}(4), 443-459.
#'
#' Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
#'
#' Li, S. (2022). \emph{The effect of estimating latent distribution using kernel density estimation method on the accuracy and efficiency of parameter estimation of item response models} [Master's thesis, Yonsei University, Seoul]. Yonsei University Library.
#'
#' Mislevy, R. J. (1984). Estimating latent distributions. \emph{Psychometrika, 49}(3), 359-381.
#'
#' Mislevy, R. J., & Bock, R. D. (1985). Implementation of the EM algorithm in the estimation of item parameters: The BILOG computer program. In D. J. Weiss (Ed.). \emph{Proceedings of the 1982 item response theory and computerized adaptive testing conference} (pp. 189-202). University of Minnesota, Department of Psychology, Computerized Adaptive Testing Conference.
#'
#' Woods, C. M., & Lin, N. (2009). Item response theory with estimation of the latent density using Davidian curves. \emph{Applied Psychological Measurement, 33}(2), 102-117.
#'
#' Woods, C. M., & Thissen, D. (2006). Item response theory with estimation of the latent population distribution using spline-based densities. \emph{Psychometrika, 71}(2), 281-301.
#'
#' @export
#'
#' @examples
#'
IRTest_Poly <- function(initialitem, data, range = c(-6,6), q = 121, model,
                        latent_dist="Normal", max_iter=200, threshold=0.0001,
                        bandwidth="nrd", h=NULL){
  Options = list(initialitem=initialitem, data=data, range=range, q=q, model=model,
                 latent_dist=latent_dist, max_iter=max_iter, threshold=threshold)
  I <- initialitem
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data)
  bw <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]
      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    Ak <- E$Ak
  }

  # Empirical histogram method
  if(latent_dist=="EHM"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      post_den <- E$fk/sum(E$fk)
      Xk <- E$Xk
      lin <- lin_inex(Xk, post_den, range = range)
      Xk <- lin$qp
      post_den <- lin$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
  }

  # Two-component normal mixture distribution
  if(latent_dist=="Mixture"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]
      M2 <- M2step(E)
      prob = M2[1];d = M2[3];sd_ratio = M2[4]
      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    Ak <- E$Ak
  }

  # Kernel density estimation method
  if(latent_dist=="KDE"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                      range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model=model)
      initialitem <- M1[[1]]

      post_den <- E$fk/sum(E$fk)
      Xk <- E$Xk
      post_den <- lin_inex(Xk, post_den, range = range)$qh
      nzindex <- round(post_den*N)!=0
      SJPI <- density(rep(Xk[nzindex], times=round(post_den*N)[nzindex]), bw = bandwidth,n=q, from = range[1], to=range[2])
      post_den <- lin_inex(Xk, SJPI$y/sum(SJPI$y), range = range)$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
    bw <- c(SJPI$bw, SJPI$n)
  }

  # Davidian curve method
  if(latent_dist=="DC"){
    phipar <- nlminb(start = rep(1,h),
                     objective = optim_phi,
                     gradient = optim_phi_grad,
                     hp=h,
                     lower = -pi/2,
                     upper = pi/2)$par

    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Poly(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Poly(E, item=initialitem, model = model)
      initialitem <- M1[[1]]

      Xk <- E$Xk
      phipar <- nlminb(start = phipar,
                       objective = DC.LL,
                       gradient = DC.grad,
                       theta=E$Xk,
                       freq = E$fk)$par

      post_den <- dcurver::ddc(x = Xk, phi = phipar)
      post_den <- post_den/sum(post_den)
      lin <- lin_inex(Xk, post_den, range = range, rule = 2)
      Xk <- lin$qp
      post_den <- lin$qh

      diff <- max(abs(I-initialitem), na.rm = T)
      I <- initialitem
      Ak <- post_den
      cat("\r","\r","Method = ",latent_dist,h,", EM cycle = ",iter,", Max-Change = ",diff,sep="")
      flush.console()
    }
  }

  # preparation for outputs
  EAP <- as.numeric(E$Pk%*%E$Xk)
  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli_Poly(initialitem, data, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(list(par_est=initialitem,
              se=M1[[2]],
              fk=E$fk,
              iter=iter,
              prob=prob,
              d=d,
              sd_ratio=sd_ratio,
              quad=Xk,
              diff=diff,
              Ak=Ak,
              Pk=E$Pk,
              theta = EAP,
              logL=-2*logL, # deviance
              bw=bw,
              Options = Options # specified argument values
  ))
}