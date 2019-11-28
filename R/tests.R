#' D - Teststatistic
#'
#' \code{D} returns the value gained by the D test
#'
#' This function computes the value of the D Test with weighth parameter l.
#'
#' @param y a numeric vector of data values.
#' @param l a positive number.
#'
#' @examples D_Henze(c(1,2,3,4),0.025)
#'
#' @return A number.
#'
#' @family teststatistic
#' @export
D_Henze <- function(y,l) {
  n = length(y)
  2/n*sum(l/(l**2+(t(matrix(rep(y,n), nrow = n, ncol = n))-y)**2))-4*sum((1+l)/((1+l)**2+y**2)) + 2*n/(2+l)
}

#' Anderson-Darling Test
#'
#' Performs the Anderson-Darling test.
#'
#' @param y a numeric vector of data values.
#' @export
AD <- function(y) {
  n=length(y)
  z= pcauchy(sort(y))
  -n - sum((2*1:n-1)*log(z) + (2*n+1-2*1:n)*log(1-z))/n
}

#' Cramer-von Mises Test
#'
#' Performs the Cramer-von Mises test.
#'
#' @param y a numeric vector of data values.
#' @export
CM <- function(y) {
  n=length(y)
  z = pcauchy(sort(y))
  sum((z-(2*1:n-1)/(2*n))**2)+1/(12*n)
}

#' Watson Test
#'
#' Performs the Watson test.
#'
#' @param y a numeric vector of data values.
#' @export
W <- function(y) {
  n=length(y)
  z = pcauchy(sort(y))
  sum((z-(2*1:n-1)/(2*n))**2)+1/(12*n) - n*(sum(z)/n-0.5)**2
}

#' Kolmogorov-Smirnov Test
#'
#' Performs the Kolmogorov-Smirnov test.
#'
#' @param y a numeric vector of data values.
#' @export
KS <- function(y) {
  n=length(y)
  z = pcauchy(sort(y))
  max(1:n/n-z,z-(0:(n-1)/n))
}

#' T1 - Teststatistic
#'
#' The function returns the value gained by the
#' \enumerate{
#'      \item T1 Test,
#'      \item T2 Test,
#'      \item T3 Test,
#'      \item T4 Test,
#'      \item T5 Test.}
#'
#' This function computes the value of the T Test with weighth parameter l. For T5 l cannot be 1!
#'
#' @param y A numeric vector.
#' @param l A positive number.
#'
#' @return A number.
#'
#' @family teststatistic
#' @export
T1 <- function(y,l=1) {
  a=l
  h1 <- function(x,a) {
    ifelse(x>0,exp(-a*x)/a,2/a-exp(a*x)/a)
  }
  h2 <- function(x,a) {
    ifelse(x>0,(a*x+1)*exp(-a*x)/a**2,-(a*x-1)*exp(a*x)/a**2)
  }
  h3 <- function(x,a) {
    ifelse(x>0,(a**2*x**2+2*a*x+2)*exp(-a*x)/a**3,4/a**3-2*exp(a*x)/a**3+2*x*exp(a*x)/a**2-x**2*exp(a*x)/a)
  }
  n <- length(y)

  A = y %*% t(y)
  B= ((1+y**2)%*%t(1+y**2))**(-1)
  Y = matrix(data =rep(y,n),byrow = FALSE,nrow=n)
  Y_t = t(Y)
  Max_Matrix = pmax(Y,Y_t)
  H1 = sapply(Max_Matrix, h1,a=a)
  H2 = sapply(Max_Matrix, h2,a=a)
  H3 = sapply(Max_Matrix, h3,a=a)
  Z=(4 * y**2%*%t(y)**2 *B - 2*Y**2 /(1+Y**2) - 2*Y_t**2 /(1+Y_t**2) + 1 ) * H1 -
    (4 * A * (Y+Y_t) * B - 2*Y /(1+Y**2) - 2*Y_t /(1+Y_t**2)) * H2 +
    (4 * A * B) * H3
  sum(Z)/n
}
#' @describeIn T1 T2 - Statistic
#' @export
T2 <- function(y,l=1) {
  a=l
  g1 <- function(x,a) {
    ifelse(x >= 0, exp(-a*x)/a,2/a- exp(a*x)/a)
  }
  g2 <- function(x,a) {
    ifelse(x >= 0,(x*a+1)/a**2*exp(-a*x),(-x*a+1)/a**2*exp(a*x))
  }
  g3 <- function(x,a) {
    ifelse(x >= 0, (x**2/a+2*x/a**2+2/a**3)*exp(-a*x),4/a**3-(x**2/a-2*x/a**2+2/a**3)*exp(a*x))
  }
  n=length(y)
  s=0
  for (j in 1:n) {
    for (k in 1:n) {
      s=s+ 4*y[j]*y[k]/((1+y[j]**2)*(1+y[k]**2)) * ( y[j]*y[k]*g1(max(y[j],y[k]),a) - (y[j]+y[k])* g2(max(y[j],y[k]),a) + g3(max(y[j],y[k]),a)) -
        4*y[j]/(1+y[j]**2) * (y[j]/2*g1(y[j],a) - g2(y[j],a)/2 + y[j]/pi * integrate(f = function(t) atan(t)*exp(-a*abs(t)),y[j],10e30,rel.tol = 1e-10)$value -1/pi * integrate(f = function(t) t * atan(t)*exp(-a*abs(t)),y[j],10e30,rel.tol = 1e-10)$value)
    }

  }
  s/n+n/(a*2)+2*n/pi**2*integrate(f = function(t) atan(t)**2*exp(-a*t),0,10e30,rel.tol = 1e-10)$value
}
#' @describeIn T1 T3 - Statistic
#' @export
T3 <- function(y,l=1) {
  a<-l
  n=length(y)
  B= ((1+y**2)%*%t(1+y**2))**(-1)
  Y = matrix(data =rep(y,n),byrow = FALSE,nrow=n)
  Y_t = t(Y)
  sum(4/((Y-Y_t)^2+a^2)^3 *( 2*a*  y%*%t(y) *B *((Y-Y_t)^2+a^2)^2 + 2*Y*a*(Y-Y_t)*((Y-Y_t)^2+a^2) / (1+Y**2) + 2*Y_t*a*(Y_t-Y)*((Y_t-Y)^2+a^2) /(1+Y_t**2) +a^3 -3*a*((Y-Y_t)^2)))/n
}
#' @describeIn T1 T4 - Statistic
#' @export
T4 <- function(y,l=1) {
  a<- l
  n=length(y)
  B= ((1+y**2)%*%t(1+y**2))**(-1)
  Y = matrix(data =rep(y,n),byrow = FALSE,nrow=n)
  Y_t = t(Y)
  sum(8*a*y%*%t(y) *B /((Y-Y_t)^2+a^2))/n + n*4/(2+a)^3 + 16*(a+1)*sum(y**2/((y^2+(a+1)^2)^2*(1+y**2)))
}
#' @describeIn T1 T5 - Statistic
T5 <- function(y,l=1) {
  a <- l
  n=length(y)
  A = y %*% t(y)
  B= ((1+y**2)%*%t(1+y**2))**(-1)
  Y = matrix(data =rep(y,n),byrow = FALSE,nrow=n)
  Y_t = t(Y)
  Max_Matrix = pmax(Y,Y_t)
  (2/n * sum(y/(1+y**2))**2 - 4 /(pi * n)* sum(y%*%t(y) *B * atan(Max_Matrix/a)) + 2 / (pi**2 * (a**2 - 1)) * sum(y/(1+y^2) * ((a-1) *pi - 2 * a * atan(y) + 2 * atan(y/a))) + n*(a+2)/(pi**2 * 2*(a+1)**2))
}


#' Kullback-Leibler Test
#'
#' Performs a test based on the Kullback-Leibler divergence.
#'
#' @param Y a numeric vector of data values.
#' @export
KL <- function(Y) {
  n=length(Y)
  b = 1/n*sum(log(dcauchy(Y)))
  f <- function(x) {
    1/(n*1.06*sd(Y)*n**(-1/5))*sum(dnorm((x-Y)/(1.06*sd(Y)*n**(-1/5))))
  }
  HB <- function(y) {
    -1/n*sum(log(sapply(y,f)))
  }
  exp(-HB(Y)-b)
}
