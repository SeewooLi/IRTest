#' logLikeli
#'
#'
#' @return
#' @export
#'
#' @examples
#'
logLikeli <- function(item, data, theta, model){
  if(length(theta)!=1){
    # if all of the examinees' ability values are specified
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
    for(i in 1:ncol(data)){
      for(j in 1:nrow(data)){
        L[j,i] <- data[j,i]*log(P(theta = theta[j],a=item[i,1],b=item[i,2]))+
          (1-data[j,i])*log(1-P(theta = theta[j],a=item[i,1],b=item[i,2]))
      }
    }
  }else{
    # if a single ability value is assumed for all
    L <- matrix(nrow = nrow(data), ncol = ncol(data))
    pvector <- t(P(theta = theta,a=item[,1],b=item[,2],c=item[,3]))

    lp1 <- log(pvector)
    lp2 <- log(1-pvector)
    lp1[lp1==-Inf] <- -.Machine$double.xmax # to avoid NaN
    lp2[lp2==-Inf] <- -.Machine$double.xmax # to avoid NaN

    L <- tcrossprod(data, lp1)+tcrossprod((1-data), lp2)
    # L is an N times 1 vector,
    # each element of which refers to an individual's log-likelihood
  }
  return(L)
}
