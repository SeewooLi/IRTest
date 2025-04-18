#' Item and ability parameters estimation for continuous response items
#'
#' @description This function estimates IRT item and ability parameters when all items are scored continuously.
#' Based on Bock & Aitkin's (1981) marginal maximum likelihood and EM algorithm (EM-MML), this function provides several latent distribution estimation algorithms which could free the normality assumption on the latent variable.
#' If the normality assumption is violated, application of these latent distribution estimation methods could reflect non-normal characteristics of the unknown true latent distribution,
#' thereby providing more accurate parameter estimates (Li, 2021; Woods & Lin, 2009; Woods & Thissen, 2006).
#'
#' @importFrom stats density nlminb
#' @importFrom utils flush.console
#'
#' @param data A matrix or data frame of item responses where responses are coded as 0 or 1.
#' Rows and columns indicate examinees and items, respectively.
#' @param model A scalar or vector that represents types of item characteristic functions:
#' \code{1}, \code{"1PL"}, \code{"Rasch"}, or \code{"RASCH"} for one-parameter logistic model,
#' and \code{2}, \code{"2PL"} for two-parameter logistic model.
#' @param range Range of the latent variable to be considered in the quadrature scheme.
#' The default is from \code{-6} to \code{6}: \code{c(-6, 6)}.
#' @param q A numeric value that represents the number of quadrature points. The default value is 121.
#' @param initialitem A matrix of initial item parameter values for starting the estimation algorithm. The default value is \code{NULL}.
#' @param ability_method The ability parameter estimation method.
#' The available options are Expected \emph{a posteriori} (\code{EAP}), Maximum Likelihood Estimates (\code{MLE}), and weighted likelihood estimates (\code{WLE}).
#' The default is \code{EAP}.
#' @param latent_dist A character string that determines latent distribution estimation method.
#' Insert \code{"Normal"}, \code{"normal"}, or \code{"N"} for the normality assumption on the latent distribution,
#' \code{"EHM"} for empirical histogram method (Mislevy, 1984; Mislevy & Bock, 1985),
#' \code{"2NM"} or \code{"Mixture"} for using two-component Gaussian mixture distribution (Li, 2021; Mislevy, 1984),
#' \code{"DC"} or \code{"Davidian"} for Davidian-curve method (Woods & Lin, 2009),
#' \code{"KDE"} for kernel density estimation method (Li, 2022),
#' and \code{"LLS"} for log-linear smoothing method (Casabianca & Lewis, 2015).
#' The default value is set to \code{"Normal"} to follow the convention.
#' @param max_iter A numeric value that determines the maximum number of iterations in the EM-MML.
#' The default value is 200.
#' @param threshold A numeric value that determines the threshold of EM-MML convergence.
#' A maximum item parameter change is monitored and compared with the threshold.
#' The default value is 0.0001.
#' @param bandwidth A character value that can be used if \code{latent_dist = "KDE"}.
#' This argument determines the bandwidth estimation method for \code{"KDE"}.
#' The default value is \code{"SJ-ste"}. See \code{\link{density}} for available options.
#' @param h A natural number less than or equal to 10 if \code{latent_dist = "DC" or "LLS"}.
#' This argument determines the complexity of the distribution.
#'
#' @details
#' \describe{
#' \item{
#' The probability of a response \eqn{u=x}, where \eqn{0<u<1}
#' }{
#' \deqn{P(u=x | a, b, \nu) = \frac{1}{B(\mu\nu, \,\nu(1-\mu))} u^{\mu\nu-1} (1-u)^{\nu(1-\mu)-1}}
#'
#' where \eqn{\mu = \frac{e^{a(\theta -b)}}{1+e^{a(\theta -b)}}}.
#' }
#'
#' \item{
#' Latent distribution estimation methods
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
#' \deqn{P(\theta=X)=\left\{\sum_{\lambda=0}^{h}{{m}_{\lambda}{X}^{\lambda}}\right\}^{2}\phi(X; 0, 1)}
#' where \eqn{h} corresponds to the argument \code{h} and determines the degree of the polynomial.
#'
#' 4) Kernel density estimation method
#' \deqn{P(\theta=X)=\frac{1}{Nh}\sum_{j=1}^{N}{K\left(\frac{X-\theta_j}{h}\right)}}
#' where \eqn{N} is the number of examinees, \eqn{\theta_j} is \eqn{j}th examinee's ability parameter,
#' \eqn{h} is the bandwidth which corresponds to the argument \code{bandwidth}, and \eqn{K( \cdot )} is a kernel function.
#' The Gaussian kernel is used in this function.
#'
#' 5) Log-linear smoothing method
#' \deqn{P(\theta=X_{q})=\exp{\left(\beta_{0}+\sum_{m=1}^{h}{\beta_{m}X_{q}^{m}}\right)}}
#' where \eqn{h} is the hyper parameter which determines the smoothness of the density, and \eqn{\theta} can take total \eqn{Q} finite values (\eqn{X_1, \dots ,X_q, \dots, X_Q}).
#' }
#' }
#'
#'
#' @return This function returns a \code{list} of several objects:
#' \item{par_est}{The item parameter estimates.}
#' \item{se}{The asymptotic standard errors for item parameter estimates.}
#' \item{fk}{The estimated frequencies of examinees at quadrature points.}
#' \item{iter}{The number of EM-MML iterations elapsed for the convergence.}
#' \item{quad}{The location of quadrature points.}
#' \item{diff}{The final value of the monitored maximum item parameter change.}
#' \item{Ak}{The estimated discrete latent distribution.
#' It is discrete (i.e., probability mass function) by the quadrature scheme.}
#' \item{Pk}{The posterior probabilities of examinees at quadrature points.}
#' \item{theta}{The estimated ability parameter values. If \code{ability_method = "MLE"}, the function returns \eqn{\pm}\code{Inf} for all or none correct answers.}
#' \item{theta_se}{Standard error of ability estimates. The asymptotic standard errors for \code{ability_method = "MLE"} (the function returns \code{NA} for all or none correct answers).
#' The standard deviations of the posterior distributions for \code{ability_method = "MLE"}.}
#' \item{logL}{The deviance (i.e., -2log\emph{L}).}
#' \item{density_par}{The estimated density parameters.}
#' \item{Options}{A replication of input arguments and other information.}
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Bock, R. D., & Aitkin, M. (1981). Marginal maximum likelihood estimation of item parameters: Application of an EM algorithm. \emph{Psychometrika, 46}(4), 443-459.
#'
#' Casabianca, J. M., & Lewis, C. (2015). IRT item parameter recovery with marginal maximum likelihood estimation using loglinear smoothing models. \emph{Journal of Educational and Behavioral Statistics, 40}(6), 547-578.
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
#' \donttest{
#' # Generating a continuous item response data
#' data <- DataGeneration(N = 1000, nitem_C = 10)$data_C
#'
#' # Analysis
#' M1 <- IRTest_Cont(data, max_iter = 3) # increase `max_iter` in real analyses.
#' }
#'
IRTest_Cont <- function(data, model=2, range = c(-6,6), q = 121, initialitem=NULL,
                        ability_method = 'EAP', latent_dist="Normal", max_iter=200,
                        threshold=0.0001, bandwidth="SJ-ste", h=NULL){

  if(is.null(initialitem)){
    initialitem <- matrix(rep(c(1,0,10), each = ncol(data)), ncol = 3)
  }
  if(!(model %in% c(1,"1PL", "Rasch", "RASCH", 2, "2PL"))) stop("Please specify the model.")

  if(model %in% c(1,"1PL", "Rasch", "RASCH")){
    model <- 1
  }else if(model %in% c(2, "2PL")){
    model <- 2
  }
  Options = list(initialitem=initialitem, data=data, model=model, range=range, q=q,
                 ability_method=ability_method,latent_dist=latent_dist,
                 max_iter=max_iter, threshold=threshold,bandwidth=bandwidth, h=h#,categories=categories
                 )

  I <- initialitem
  Xk <- seq(range[1],range[2],length=q)
  Ak <- dist2(Xk, 0.5, 0, 1)/sum(dist2(Xk, 0.5, 0, 1))
  iter <- 0
  diff <- 1
  prob = 0.5
  d = 1
  sd_ratio = 1
  N = nrow(data)
  density_par <- NULL

  # Normality assumption method
  if(latent_dist %in% c("Normal", "normal", "N")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item = initialitem, data = data, range = range, q = q, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      if(model == 1){
        ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range)
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- (initialitem[,2]-ld_est$m)/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
        )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    Ak <- E$Ak
  }

  # Empirical histogram method
  if(latent_dist=="EHM"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(model == 1){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- (initialitem[,2]-ld_est$m)/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
      )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # Two-component normal mixture distribution
  if(latent_dist %in% c("Mixture", "2NM")){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=prob, d=d, sd_ratio=sd_ratio, range = range)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      M2 <- M2step(E)
      prob = M2$prob; d = M2$d; sd_ratio = M2$sd_ratio

      if(model == 1){
        initialitem[,1] <- initialitem[,1]*M2$s
        initialitem[,2] <- (initialitem[,2]-M2$m)/M2$s
        M1[[2]][,2] <- M1[[2]][,2]/M2$s
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
      )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    Ak <- E$Ak
    density_par <- list(prob=prob, d=d, sd_ratio=sd_ratio)
  }

  # Kernel density estimation method
  if(latent_dist=="KDE"){
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                 range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, bandwidth=bandwidth, N=N, q=q)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(model == 1){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- (initialitem[,2]-ld_est$m)/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
      )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
    density_par <- ld_est$par
  }

  # Davidian curve method
  if(latent_dist%in% c("DC", "Davidian")){
    density_par <- nlminb(start = rep(1,h),
                          objective = optim_phi,
                          gradient = optim_phi_grad,
                          hp=h)$par

    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, par=density_par,N=N)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density
      density_par <- ld_est$par

      if(model == 1){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- (initialitem[,2]-ld_est$m)/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
      )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # Log-linear smoothing method
  if(latent_dist=="LLS"){
    density_par <- rep(0, h)
    while(iter < max_iter & diff > threshold){
      iter <- iter +1

      E <- Estep_Cont(item=initialitem, data=data, q=q, prob=0.5, d=0, sd_ratio=1,
                 range=range, Xk=Xk, Ak=Ak)
      M1 <- Mstep_Cont(E, initialitem, data, model)
      initialitem <- M1[[1]]

      ld_est <- latent_dist_est(method = latent_dist, Xk = E$Xk, posterior = E$fk, range=range, par=density_par, N=N)
      Xk <- ld_est$Xk
      Ak <- ld_est$posterior_density

      if(model == 1){
        initialitem[,1] <- initialitem[,1]*ld_est$s
        initialitem[,2] <- (initialitem[,2]-ld_est$m)/ld_est$s
        M1[[2]][,2] <- M1[[2]][,2]/ld_est$s
        if(iter>3){
          density_par <- ld_est$par
        }
      } else {
        density_par <- ld_est$par
      }

      diff <- max(
        max(abs(I[,-3]-initialitem[,-3]), na.rm = TRUE),
        max(abs(log(I[,3])-log(initialitem[,3])), na.rm = TRUE),
        na.rm = TRUE
      )
      I <- initialitem
      message("\r","\r","Method = ",latent_dist,", EM cycle = ",iter,", Max-Change = ",diff,sep="",appendLF=FALSE)
      flush.console()
    }
  }

  # ability parameter estimation
  if(is.null(ability_method)){
    theta <- NULL
    theta_se <- NULL
  } else if(ability_method == 'EAP'){
    theta <- as.numeric(E$Pk%*%E$Xk)
    theta_se <- sqrt(as.numeric(E$Pk%*%(E$Xk^2))-theta^2)
  } else if(ability_method == 'MLE'){
    mle_result <- MLE_theta(item = initialitem, data = data, type = "cont")
    theta <- mle_result[[1]]
    theta_se <- mle_result[[2]]
  } else if(ability_method == 'WLE'){
    wle_result <- WLE_theta(item = initialitem, data = data, type = "cont")
    theta <- wle_result[[1]]
    theta_se <- wle_result[[2]]
  }
  dn <- list(colnames(data),c("a", "b", "nu"))
  dimnames(initialitem) <- dn
  dimnames(M1[[2]]) <- list(colnames(data),c("a", "b", "log(nu)"))

  # preparation for outputs

  logL <- 0
  for(i in 1:q){
    logL <- logL+sum(logLikeli_Cont(initialitem, data, theta = Xk[i])*E$Pk[,i])
  }
  E$Pk[E$Pk==0]<- .Machine$double.xmin
  Ak[Ak==0] <- .Machine$double.xmin
  logL <- logL + as.numeric(E$fk%*%log(Ak)) - sum(E$Pk*log(E$Pk))
  return(structure(
    list(par_est=initialitem,
         se=M1[[2]],
         fk=E$fk,
         iter=iter,
         quad=Xk,
         diff=diff,
         Ak=Ak,
         Pk=E$Pk,
         theta = theta,
         theta_se = theta_se,
         logL=-2*logL, # deviance
         density_par = density_par,
         Options = Options # specified argument values
    ),
    class = c("cont", "IRTest", "list")
  )
  )
}
