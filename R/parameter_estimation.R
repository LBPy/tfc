#' Parameter estimation
#'
#' \code{median_est} returns the estimated parameters of a cauchy distribution.
#'
#' This function ....
#'
#' @param x A Numeric vector.
#'
#' @return the location and the scale parameter.
#' @export
median_est <- function(x) {
  c(median(x),IQR(x)/2)
}

#' @describeIn median_est Parameter estimation with the ML Method
#' @export
ml_est <- function(x) {
  n=length(x)
  f <- function(a) {
    -n*log(a[2])+sum(log(a[2]**2+(x-a[1])**2))
  }
  optim(est_param(x),f,lower = c(-Inf,1e-10),upper = c(Inf,Inf),method = "L-BFGS-B")$par
}

#' @describeIn median_est Paremter estimation with an estimated squared integrated error.
#' @param l Parameter for the weight function described in \code{\link{D}}
#' @export
eise_est <- function(x,l) {
  n=length(x)
  f <- function(z,l) {
    a=z[1]
    b=z[2]
    (2/n**2 * sum(l*b**2/(l**2*b**2+(t(matrix(rep(x,n), nrow = n, ncol = n))-x)**2)) - 4/n*sum((1+l)*b**2/((1+l)**2*b**2+(x-a)**2)))
  }
  optim(c(est_param(x)),f,l=1,lower = c(-Inf,1e-10),upper = c(Inf,Inf),method = "L-BFGS-B")$par
}
