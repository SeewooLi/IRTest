% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information.R
\name{inform_f_item}
\alias{inform_f_item}
\title{Item information function}
\usage{
inform_f_item(x, item.matrix, item, type)
}
\arguments{
\item{x}{A vector of \eqn{\theta} value(s).}

\item{item.matrix}{A matrix of item parameters.}

\item{item}{A numeric value indicating an item.
If \eqn{n} is provided, item information is calculated for the \eqn{n}th item.}

\item{type}{A character value which determines the item type:
\code{"d"} stands for a dichotomous item, and \code{"p"} stands for a polytomous item.}
}
\value{
A vector of item information values of the same length as \code{x}.
}
\description{
Item information function
}
\author{
Seewoo Li \email{cu@yonsei.ac.kr}
}