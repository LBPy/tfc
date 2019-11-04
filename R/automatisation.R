#' Function for standardizing data
#'
#' \code{standardisiert} returns the standarized vector.
#'
#' This function standardizes the Inputvector. This is done with a parameter estimation function like \code{\link{median_est}}.
#' For  \enumerate{
#'              \item \eqn{l=1} \code{\link{median_est}} is used,
#'              \item \eqn{l=2} \code{\link{ml_est}} is used,
#'              \item \eqn{l=3} \code{\link{eise_est}} is used.
#'}
#'
#' @param x A numeric vector.
#' @param l A Parameter.
#'
#' @return A numeric vector.
#' @export

standardisiert <- function(x,method_estimation = 1) {
  if(method_estimation == 1) param = median_est(x)
  if(method_estimation == 2) param = ml_est(x)
  if(method_estimation == 3) param = eise_est(x)
  (x-param[1])/param[2]
}


#' Function for standardizing data
#'
#' distr returns a random vector simulated by a choosen distribution.
#'
#' @export
distr <- function(n,case) {
  if (case=="C(0,1)") {
    #return(list("C(0.1)"=rcauchy(n)))
    return(rcauchy(n))
  }
  if (case=="N(0,1)") {
    return(rnorm(n))
  }
  if (case=="NC(0.1,0.9)") {
    return(0.1*rnorm(n)+0.9*rcauchy(n))
  }
  if (case=="NC(0.3,0.7)") {
    return(0.3*rnorm(n)+0.7*rcauchy(n))
  }
  if (case=="NC(0.5,0.5)") {
    return(0.5*rnorm(n)+0.5*rcauchy(n))
  }
  if (case=="NC(0.7,0.3)") {
    return(0.7*rnorm(n)+0.3*rcauchy(n))
  }
  if (case=="NC(0.9,0.1)") {
    return(0.9*rnorm(n)+0.1*rcauchy(n))
  }
  if (case=="t(2)") {
    return(rt(n,2))
  }
  if (case=="t(3)") {
    return(rt(n,3))
  }
  if (case=="t(4)") {
    return(rt(n,4))
  }
  if (case=="t(5)") {
    return(rt(n,5))
  }
  if (case=="t(7)") {
    return(rt(n,7))
  }
  if (case=="t(10)") {
    return(rt(n,10))
  }
  if (case=="Stable(0.5,0)") {
    return(rstable(n,0.5,0))
  }
  if (case=="Stable(1.2,0)") {
    return(rstable(n,1.2,0))
  }
  if (case=="Stable(1.5,0)") {
    return(rstable(n,1.5,0))
  }
  if (case=="Stable(1.7,0)") {
    return(rstable(n,1.7,0))
  }
  if (case=="Stable(1.9,0)") {
    return(rstable(n,1.9,0))
  }
  if (case=="Stable(0.5,-1)") {
    return(rstable(n,0.5,-1))
  }
  if (case=="Stable(1,-1)") {
    return(rstable(n,1,-1))
  }
  if (case=="Stable(1.5,-1)") {
    return(rstable(n,1.5,-1))
  }
  if (case=="Stable(2,-1)") {
    return(rstable(n,2,-1))
  }
  if (case=="Stable(0.5,1)") {
    return(rstable(n,0.5,1))
  }
  if (case=="Stable(1,1)") {
    return(rstable(n,1,1))
  }
  if (case=="Stable(1.5,1)") {
    return(rstable(n,1.5,1))
  }
  if (case=="Stable(2,1)") {
    return(rstable(n,2,1))
  }
  if (case=="Tukey(1)") {
    Z = rnorm(n)
    return(Z*exp(Z**2/2))
  }
  if (case=="Tukey(0.2)") {
    Z = rnorm(n)
    return(Z*exp(0.2 * Z**2/2))
  }
  if (case=="Tukey(0.1)") {
    Z = rnorm(n)
    return(Z*exp( 0.1 *Z**2/2))
  }
  if (case=="Tukey(0.05)") {
    Z = rnorm(n)
    return(Z*exp(0.05 * Z**2/2))
  }
  if (case=="U(0,1)") {
    return(runif(n))
  }
  if (case=="Logistic") {
    return(rlogis(n))
  }
  if (case=="Laplace") { #Laplaca
    return(rlaplace(n))
  }
  if (case=="Gumbel") { # Gumbel
    return(rgumbel(n,0,1))
  }
  if (case==35) {
    return(rnorm(20))
  }
}

#' Function for the result of a test
#'
#' \code{testentscheid} returns 0 if \eqn{H_0} is not rejected and 1 if it is rejected.
#'
#' If \code{Verteilung} is empty then the given numeric vector x is taken for testing.
#'
#' @param n An integer. (Here 10,20 or 50)
#' @param Verteilung A String specifing the choosen distribution to test.
#' @param method A name of a test \code{\link{teststatistic}}
#' @param x A numeric vector of length n.
#'
#' @return binary number.
#' @export
testentscheid <- function(n,Verteilung ="",method,x=0){
  if (n==10) {a=1}
  if (n==20) {a=2}
  if (n==50) {a=3}
  if (n==100) {a=4}
  if (n==200) {a=5}
  if(Verteilung != ""){
  x = distr(n,Verteilung)
  }
  d<-"Error"
  if(method == "D")         {d <- D(standardisiert(x,1),5) > quantile_D_Median[a,6]}
  if(method == "D_ML")      {d<- D(standardisiert(x,2),5) > quantile_D_ML[a,6]}
  #if(method = D_EISE){}
  if(method == "D_2_Median"){d<-D_2(standardisiert(x,1)) > quantile_KL_Median[a]}
  if(method == "D_2_ML")    {d <- D_2(standardisiert(x,2)) >quantile_KL_ML[a]}
  if(method == "T1_ML")     {d <-T1(standardisiert(x,2),1) > quantile_T1_ML[a,4]}
  if(method == "T2_ML")     {d <- T2(standardisiert(x,2),1) >quantile_T2_ML[a,4]}
  if(method == "T1_Median") {d <- T1(standardisiert(x,1),1) > quantile_T1_Median[a,4]}
  if(method == "T2_Median") {d <-T2(standardisiert(x,1),1) > quantile_T2_Median[a,4]}
  if(method == "AD_ML")     {d <- AD(standardisiert(x,2)) > quantile_edf_ML[a,3]}
  if(method == "CM_ML")     {d <- CM(standardisiert(x,2)) > quantile_edf_ML[a,2]}
  if(method == "KS_ML")     {d <- KS(standardisiert(x,2)) > quantile_edf_ML[a,1]}
  if(method == "W_ML")      {d <- W(standardisiert(x,2)) > quantile_edf_ML[a,4]}
  if(method == "AD_Median") {d <- AD(standardisiert(x,1)) > quantile_edf_Median[a,3]}
  if(method == "CM_Median") {d <- CM(standardisiert(x,1)) > quantile_edf_Median[a,2]}
  if(method == "KS_Median") {d <- KS(standardisiert(x,1)) > quantile_edf_Median[a,1]}
  if(method == "W_Median")  {d <- W(standardisiert(x,1)) > quantile_edf_Median[a,4]}
  if(method == "T1_ML 0.025")     {d <-T1(standardisiert(x,2),0.025) > quantile_T1_ML[a,1]}
  if(method == "T2_ML 0.025")     {d <- T2(standardisiert(x,2),0.025) >quantile_T2_ML[a,1]}
  if(method == "T1_ML 0.1")     {d <-T1(standardisiert(x,2),0.1) > quantile_T1_ML[a,2]}
  if(method == "T2_ML 0.1")     {d <- T2(standardisiert(x,2),0.1) >quantile_T2_ML[a,2]}
  if(method == "T1_ML 0.5")     {d <-T1(standardisiert(x,2),0.5) > quantile_T1_ML[a,3]}
  if(method == "T2_ML 0.5")     {d <- T2(standardisiert(x,2),0.5) >quantile_T2_ML[a,3]}
  if(method == "T1_ML 2.5")     {d <-T1(standardisiert(x,2),2.5) > quantile_T1_ML[a,5]}
  if(method == "T2_ML 2.5")     {d <- T2(standardisiert(x,2),2.5) >quantile_T2_ML[a,5]}
  if(method == "T1_ML 5")     {d <-T1(standardisiert(x,2),5) > quantile_T1_ML[a,6]}
  if(method == "T2_ML 5")     {d <- T2(standardisiert(x,2),5) >quantile_T2_ML[a,6]}
  if(method == "T1_ML 10")     {d <-T1(standardisiert(x,2),10) > quantile_T1_ML[a,7]}
  if(method == "T2_ML 10")     {d <- T2(standardisiert(x,2),10) >quantile_T2_ML[a,7]}
  if(method == "T3_ML 0.025")     {d <-T3(standardisiert(x,2),0.025) > quantile_T3_ML[a,1]}
  if(method == "T3_ML 0.1")        {d <-T3(standardisiert(x,2),0.1) > quantile_T3_ML[a,2]}
  if(method == "T3_ML 0.5")        {d <-T3(standardisiert(x,2),0.5) > quantile_T3_ML[a,3]}
  if(method == "T3_ML 1")          {d <-T3(standardisiert(x,2),1) > quantile_T3_ML[a,4]}
  if(method == "T3_ML 2.5")        {d <-T3(standardisiert(x,2),2.5) > quantile_T3_ML[a,5]}
  if(method == "T3_ML 5")          {d <-T3(standardisiert(x,2),5) > quantile_T3_ML[a,6]}
  if(method == "T3_ML 10")         {d <-T3(standardisiert(x,2),10) > quantile_T3_ML[a,7]}
  if(method == "T3_Median 0.025")     {d <-T3(standardisiert(x,1),0.025) > quantile_T3_Median[a,1]}
  if(method == "T3_Median 0.1")        {d <-T3(standardisiert(x,1),0.1) > quantile_T3_Median[a,2]}
  if(method == "T3_Median 0.5")        {d <-T3(standardisiert(x,1),0.5) > quantile_T3_Median[a,3]}
  if(method == "T3_Median 1")          {d <-T3(standardisiert(x,1),1) > quantile_T3_Median[a,4]}
  if(method == "T3_Median 2.5")        {d <-T3(standardisiert(x,1),2.5) > quantile_T3_Median[a,5]}
  if(method == "T3_Median 5")          {d <-T3(standardisiert(x,1),5) > quantile_T3_Median[a,6]}
  if(method == "T3_Median 10")         {d <-T3(standardisiert(x,1),10) > quantile_T3_Median[a,7]}
  if(method == "T4_ML 0.025")     {d <-T4(standardisiert(x,2),0.025) > quantile_T4_ML[a,1]}
  if(method == "T4_ML 0.1")        {d <-T4(standardisiert(x,2),0.1) > quantile_T4_ML[a,2]}
  if(method == "T4_ML 0.5")        {d <-T4(standardisiert(x,2),0.5) > quantile_T4_ML[a,3]}
  if(method == "T4_ML 1")          {d <-T4(standardisiert(x,2),1) > quantile_T4_ML[a,4]}
  if(method == "T4_ML 2.5")        {d <-T4(standardisiert(x,2),2.5) > quantile_T4_ML[a,5]}
  if(method == "T4_ML 5")          {d <-T4(standardisiert(x,2),5) > quantile_T4_ML[a,6]}
  if(method == "T4_ML 10")         {d <-T4(standardisiert(x,2),10) > quantile_T4_ML[a,7]}
  if(method == "T4_Median 0.025")     {d <-T4(standardisiert(x,1),0.025) > quantile_T4_Median[a,1]}
  if(method == "T4_Median 0.1")        {d <-T4(standardisiert(x,1),0.1) > quantile_T4_Median[a,2]}
  if(method == "T4_Median 0.5")        {d <-T4(standardisiert(x,1),0.5) > quantile_T4_Median[a,3]}
  if(method == "T4_Median 1")          {d <-T4(standardisiert(x,1),1) > quantile_T4_Median[a,4]}
  if(method == "T4_Median 2.5")        {d <-T4(standardisiert(x,1),2.5) > quantile_T4_Median[a,5]}
  if(method == "T4_Median 5")          {d <-T4(standardisiert(x,1),5) > quantile_T4_Median[a,6]}
  if(method == "T4_Median 10")         {d <-T4(standardisiert(x,1),10) > quantile_T4_Median[a,7]}
  if(method == "T5_ML 0.025")     {d <-T5(standardisiert(x,2),0.025) > quantile_T5_ML[a,1]}
  if(method == "T5_ML 0.1")        {d <-T5(standardisiert(x,2),0.1) > quantile_T5_ML[a,2]}
  if(method == "T5_ML 0.5")        {d <-T5(standardisiert(x,2),0.5) > quantile_T5_ML[a,3]}
  if(method == "T5_ML 2.5")          {d <-T5(standardisiert(x,2),2.5) > quantile_T5_ML[a,4]}
  if(method == "T5_ML 5")        {d <-T5(standardisiert(x,2),5) > quantile_T5_ML[a,5]}
  if(method == "T5_ML 10")          {d <-T5(standardisiert(x,2),10) > quantile_T5_ML[a,6]}
  if(method == "T5_ML 15")         {d <-T5(standardisiert(x,2),15) > quantile_T5_ML[a,7]}
  if(method == "T5_Median 0.025")     {d <-T5(standardisiert(x,1),0.025) > quantile_T5_Median[a,1]}
  if(method == "T5_Median 0.1")        {d <-T5(standardisiert(x,1),0.1) > quantile_T5_Median[a,2]}
  if(method == "T5_Median 0.5")        {d <-T5(standardisiert(x,1),0.5) > quantile_T5_Median[a,3]}
  if(method == "T5_Median 2.5")          {d <-T5(standardisiert(x,1),2.5) > quantile_T5_Median[a,4]}
  if(method == "T5_Median 5")        {d <-T5(standardisiert(x,1),5) > quantile_T5_Median[a,5]}
  if(method == "T5_Median 10")          {d <-T5(standardisiert(x,1),10) > quantile_T5_Median[a,6]}
  if(method == "T5_Median 15")         {d <-T5(standardisiert(x,1),15) > quantile_T5_Median[a,7]}
  return(list("Decision" = d))

}
testentscheid_Schaubild <- function(n,Verteilung,Parameter,method){
  x = distr(n,Verteilung)
  d<-"Error"
  a = seq(0.1,13,0.2)
  if(method == "T1_ML")     {d <-T1(standardisiert(x,2),a[Parameter]) > quantile_T1_ML_Param[Parameter]}
  if(method == "T2_ML")     {d <- T2(standardisiert(x,2),a[Parameter]) >quantile_T2_ML_Param[Parameter]}
  if(method == "T1_Median") {d <- T1(standardisiert(x,1),a[Parameter]) > quantile_T1_Median_Param[Parameter]}
  if(method == "T2_Median") {d <-T2(standardisiert(x,1),a[Parameter]) > quantile_T2_Median_Param[Parameter]}
  if(method == "T3_ML")     {d <-T3(standardisiert(x,2),a[Parameter]) > quantile_T3_ML_Param[Parameter]}
  if(method == "T4_ML")     {d <- T4(standardisiert(x,2),a[Parameter]) >quantile_T4_ML_Param[Parameter]}
  return(list("Decision" = d))
}
testentscheid_t_Schaubild <- function(n,Verteilung,method){
  if (n==10) {a=1}
  if (n==20) {a=2}
  if (n==50) {a=3}
  if (n==100) {a=4}
  if (n==200) {a=5}
  x = rt(n,Verteilung)
  d<-"Error"
  if(method == "D")         {d <- D(standardisiert(x,1),5) > quantile_D_Median[a,6]}
  if(method == "D_2_Median") {d<-D_2(standardisiert(x,1)) > quantile_KL_Median[a]}
  if(method == "T1_ML 0.025")     {d <-T1(standardisiert(x,2),0.025) > quantile_T1_ML[a,1]}
  if(method == "T2_ML 0.025")     {d <- T2(standardisiert(x,2),0.025) >quantile_T2_ML[a,1]}
  if(method == "T3_ML 10")     {d <-T3(standardisiert(x,2),10) > quantile_T3_ML[a,7]}
  if(method == "T4_ML 10")     {d <- T4(standardisiert(x,2),10) >quantile_T4_ML[a,7]}
  return(list("Decision" = d))
}
