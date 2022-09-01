% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/original_par_2GM.R
\name{original_par_2GM}
\alias{original_par_2GM}
\title{Recovering original parameters of two-component Gaussian mixture distribution from re-parameterized parameters}
\usage{
original_par_2GM(
  prob = 0.5,
  d = 0,
  sd_ratio = 1,
  overallmean = 0,
  overallsd = 1
)
}
\arguments{
\item{prob}{A numeric value of \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees who belong to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).}

\item{d}{A numeric value of \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
where \eqn{\mu_1} is the estimated mean of the first Gaussian component,
\eqn{\mu_2} is the estimated mean of the second Gaussian component,
and \eqn{\bar{\sigma}} is the standard deviation of the latent distribution (Li, 2021).
Without loss of generality, \eqn{\mu_2 \ge \mu_1}, thus \eqn{\delta \ge 0}, is assumed.}

\item{sd_ratio}{A numeric value of \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} is the estimated standard deviation of the first Gaussian component, \eqn{\sigma_2} is the estimated standard deviation of the second Gaussian component (Li, 2021).}

\item{overallmean}{A numeric value of \eqn{\bar{\mu}} that determines the overall mean of two-component Gaussian mixture distribution.}

\item{overallsd}{A numeric value of \eqn{\bar{\sigma}} that determines the overall standard deviation of two-component Gaussian mixture distribution.}
}
\value{
This function returns a vector of length 4: \code{c(m1,m2,s1,s2)}.
\item{m1}{The location parameter (mean) of the first Gaussian component.}
\item{m2}{The location parameter (mean) of the second Gaussian component.}
\item{s1}{The scale parameter (standard deviation) of the first Gaussian component.}
\item{s2}{The scale parameter (standard deviation) of the second Gaussian component.}
}
\description{
Recovering original parameters of two-component Gaussian mixture distribution from re-parameterized parameters
}
\details{
\describe{
\item{The original two-component Gaussian mixture distribution}{
\deqn{f(x)=\pi\times \phi(x | \mu_1, \sigma_1)+(1-\pi)\times \phi(x | \mu_2, \sigma_2)}
, where \eqn{\phi} is a Gaussian component.
}
\item{The re-parameterized two-component Gaussian mixture distribution}{
\deqn{f(x)=2GM(x|\pi, \delta, \zeta, \bar{\mu}, \bar{\sigma})}
, where \eqn{\bar{\mu}} is overall mean and \eqn{\bar{\sigma}} is overall standard deviation of the distribution.
}

\item{
The original parameters of two-component Gaussian mixture distribution can be retrieved as follows;}{
1) Mean of the first Gaussian component (\code{m1}).
\deqn{\mu_1=-(1-\pi)\delta\bar{\sigma}+\bar{\mu}}

2) Mean of the second Gaussian component (\code{m2}).
\deqn{\mu_2=\pi\delta\bar{\sigma}+\bar{\mu}}

3) Standard deviation of the first Gaussian component (\code{s1}).
\deqn{\sigma_1^2=\bar{\sigma}^2\left(\frac{1-\pi(1-\pi)\delta^2}{\pi+(1-\pi)\zeta^2}\right)}

4) Standard deviation of the second Gaussian component (\code{s2}).
\deqn{\sigma_1^2=\bar{\sigma}^2\left(\frac{1-\pi(1-\pi)\delta^2}{\frac{1}{\zeta^2}\pi+(1-\pi)}\right)=\zeta^2\sigma_1^2}
}
}
}
\references{
Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
}
\author{
Seewoo Li \email{cu@yonsei.ac.kr}
}