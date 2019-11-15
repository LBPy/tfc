#' Function for standardizing data
#'
#' \code{standardisiert} returns the standarized vector.
#'
#' This function standardizes the Inputvector. This is done with a parameter estimation function like \code{\link{median_est}}.
#' For  \enumerate{
#'              \item \eqn{method_estimation=1} \code{\link{median_est}} is used,
#'              \item \eqn{method_estimation=2} \code{\link{ml_est}} is used,
#'              \item \eqn{method_estimation=3} \code{\link{eise_est}} is used.
#'}
#'
#' @param x A numeric vector.
#' @param method_estimation A Parameter.
#'
#' @return A numeric vector.
#' @export

standardisiert <- function(x,method_estimation = 1) {
  if(method_estimation == 1) param = tfc::median_est(x)
  if(method_estimation == 2) param = tfc::ml_est(x)
  if(method_estimation == 3) param = tfc::eise_est(x)
  (x-param[1])/param[2]
}


#' Function for standardizing data
#'
#' distr returns a random vector simulated by a choosen distribution.
#'
#' @param n The size of the dataset (10,20 or 50).
#' @param case A name of a distribution.
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
    return(stabledist::rstable(n,0.5,0))
  }
  if (case=="Stable(1.2,0)") {
    return(stabledist::rstable(n,1.2,0))
  }
  if (case=="Stable(1.5,0)") {
    return(stabledist::rstable(n,1.5,0))
  }
  if (case=="Stable(1.7,0)") {
    return(stabledist::rstable(n,1.7,0))
  }
  if (case=="Stable(1.9,0)") {
    return(stabledist::rstable(n,1.9,0))
  }
  if (case=="Stable(0.5,-1)") {
    return(stabledist::rstable(n,0.5,-1))
  }
  if (case=="Stable(1,-1)") {
    return(stabledist::rstable(n,1,-1))
  }
  if (case=="Stable(1.5,-1)") {
    return(stabledist::rstable(n,1.5,-1))
  }
  if (case=="Stable(2,-1)") {
    return(stabledist::rstable(n,2,-1))
  }
  if (case=="Stable(0.5,1)") {
    return(stabledist::rstable(n,0.5,1))
  }
  if (case=="Stable(1,1)") {
    return(stabledist::rstable(n,1,1))
  }
  if (case=="Stable(1.5,1)") {
    return(stabledist::rstable(n,1.5,1))
  }
  if (case=="Stable(2,1)") {
    return(stabledist::rstable(n,2,1))
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
    return(rmutil::rlaplace(n))
  }
  if (case=="Gumbel") { # Gumbel
    return(actuar::rgumbel(n,0,1))
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
#' @param method A name of a test.
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
  if(method == "D")         {d <- D_Henze(standardisiert(x,1),5) > tfc::quantile_D_Median[a,6]}
  if(method == "D_ML")      {d<- D_Henze(standardisiert(x,2),5) > tfc::quantile_D_ML[a,6]}
  #if(method = D_EISE){}
  if(method == "D_2_Median"){d<-D_2(standardisiert(x,1)) > tfc::quantile_KL_Median[a]}
  if(method == "D_2_ML")    {d <- D_2(standardisiert(x,2)) >tfc::quantile_KL_ML[a]}
  if(method == "T1_ML")     {d <-T1(standardisiert(x,2),1) > tfc::quantile_T1_ML[a,4]}
  if(method == "T2_ML")     {d <- T2(standardisiert(x,2),1) >tfc::quantile_T2_ML[a,4]}
  if(method == "T1_Median") {d <- T1(standardisiert(x,1),1) > tfc::quantile_T1_Median[a,4]}
  if(method == "T2_Median") {d <-T2(standardisiert(x,1),1) > tfc::quantile_T2_Median[a,4]}
  if(method == "AD_ML")     {d <- AD(standardisiert(x,2)) > tfc::quantile_edf_ML[a,3]}
  if(method == "CM_ML")     {d <- CM(standardisiert(x,2)) > tfc::quantile_edf_ML[a,2]}
  if(method == "KS_ML")     {d <- KS(standardisiert(x,2)) > tfc::quantile_edf_ML[a,1]}
  if(method == "W_ML")      {d <- W(standardisiert(x,2)) > tfc::quantile_edf_ML[a,4]}
  if(method == "AD_Median") {d <- AD(standardisiert(x,1)) > tfc::quantile_edf_Median[a,3]}
  if(method == "CM_Median") {d <- CM(standardisiert(x,1)) > tfc::quantile_edf_Median[a,2]}
  if(method == "KS_Median") {d <- KS(standardisiert(x,1)) > tfc::quantile_edf_Median[a,1]}
  if(method == "W_Median")  {d <- W(standardisiert(x,1)) > tfc::quantile_edf_Median[a,4]}
  if(method == "T1_ML 0.025")     {d <-T1(standardisiert(x,2),0.025) > tfc::quantile_T1_ML[a,1]}
  if(method == "T2_ML 0.025")     {d <- T2(standardisiert(x,2),0.025) >tfc::quantile_T2_ML[a,1]}
  if(method == "T1_ML 0.1")     {d <-T1(standardisiert(x,2),0.1) > tfc::quantile_T1_ML[a,2]}
  if(method == "T2_ML 0.1")     {d <- T2(standardisiert(x,2),0.1) >tfc::quantile_T2_ML[a,2]}
  if(method == "T1_ML 0.5")     {d <-T1(standardisiert(x,2),0.5) > tfc::quantile_T1_ML[a,3]}
  if(method == "T2_ML 0.5")     {d <- T2(standardisiert(x,2),0.5) >tfc::quantile_T2_ML[a,3]}
  if(method == "T1_ML 2.5")     {d <-T1(standardisiert(x,2),2.5) > tfc::quantile_T1_ML[a,5]}
  if(method == "T2_ML 2.5")     {d <- T2(standardisiert(x,2),2.5) >tfc::quantile_T2_ML[a,5]}
  if(method == "T1_ML 5")     {d <-T1(standardisiert(x,2),5) > tfc::quantile_T1_ML[a,6]}
  if(method == "T2_ML 5")     {d <- T2(standardisiert(x,2),5) >tfc::quantile_T2_ML[a,6]}
  if(method == "T1_ML 10")     {d <-T1(standardisiert(x,2),10) > tfc::quantile_T1_ML[a,7]}
  if(method == "T2_ML 10")     {d <- T2(standardisiert(x,2),10) >tfc::quantile_T2_ML[a,7]}
  if(method == "T3_ML 0.025")     {d <-T3(standardisiert(x,2),0.025) > tfc::quantile_T3_ML[a,1]}
  if(method == "T3_ML 0.1")        {d <-T3(standardisiert(x,2),0.1) > tfc::quantile_T3_ML[a,2]}
  if(method == "T3_ML 0.5")        {d <-T3(standardisiert(x,2),0.5) > tfc::quantile_T3_ML[a,3]}
  if(method == "T3_ML 1")          {d <-T3(standardisiert(x,2),1) > tfc::quantile_T3_ML[a,4]}
  if(method == "T3_ML 2.5")        {d <-T3(standardisiert(x,2),2.5) > tfc::quantile_T3_ML[a,5]}
  if(method == "T3_ML 5")          {d <-T3(standardisiert(x,2),5) > tfc::quantile_T3_ML[a,6]}
  if(method == "T3_ML 10")         {d <-T3(standardisiert(x,2),10) > tfc::quantile_T3_ML[a,7]}
  if(method == "T3_Median 0.025")     {d <-T3(standardisiert(x,1),0.025) > tfc::quantile_T3_Median[a,1]}
  if(method == "T3_Median 0.1")        {d <-T3(standardisiert(x,1),0.1) > tfc::quantile_T3_Median[a,2]}
  if(method == "T3_Median 0.5")        {d <-T3(standardisiert(x,1),0.5) > tfc::quantile_T3_Median[a,3]}
  if(method == "T3_Median 1")          {d <-T3(standardisiert(x,1),1) > tfc::quantile_T3_Median[a,4]}
  if(method == "T3_Median 2.5")        {d <-T3(standardisiert(x,1),2.5) > tfc::quantile_T3_Median[a,5]}
  if(method == "T3_Median 5")          {d <-T3(standardisiert(x,1),5) > tfc::quantile_T3_Median[a,6]}
  if(method == "T3_Median 10")         {d <-T3(standardisiert(x,1),10) > tfc::quantile_T3_Median[a,7]}
  if(method == "T4_ML 0.025")     {d <-T4(standardisiert(x,2),0.025) > tfc::quantile_T4_ML[a,1]}
  if(method == "T4_ML 0.1")        {d <-T4(standardisiert(x,2),0.1) > tfc::quantile_T4_ML[a,2]}
  if(method == "T4_ML 0.5")        {d <-T4(standardisiert(x,2),0.5) > tfc::quantile_T4_ML[a,3]}
  if(method == "T4_ML 1")          {d <-T4(standardisiert(x,2),1) > tfc::quantile_T4_ML[a,4]}
  if(method == "T4_ML 2.5")        {d <-T4(standardisiert(x,2),2.5) > tfc::quantile_T4_ML[a,5]}
  if(method == "T4_ML 5")          {d <-T4(standardisiert(x,2),5) > tfc::quantile_T4_ML[a,6]}
  if(method == "T4_ML 10")         {d <-T4(standardisiert(x,2),10) > tfc::quantile_T4_ML[a,7]}
  if(method == "T4_Median 0.025")     {d <-T4(standardisiert(x,1),0.025) > tfc::quantile_T4_Median[a,1]}
  if(method == "T4_Median 0.1")        {d <-T4(standardisiert(x,1),0.1) > tfc::quantile_T4_Median[a,2]}
  if(method == "T4_Median 0.5")        {d <-T4(standardisiert(x,1),0.5) > tfc::quantile_T4_Median[a,3]}
  if(method == "T4_Median 1")          {d <-T4(standardisiert(x,1),1) > tfc::quantile_T4_Median[a,4]}
  if(method == "T4_Median 2.5")        {d <-T4(standardisiert(x,1),2.5) > tfc::quantile_T4_Median[a,5]}
  if(method == "T4_Median 5")          {d <-T4(standardisiert(x,1),5) > tfc::quantile_T4_Median[a,6]}
  if(method == "T4_Median 10")         {d <-T4(standardisiert(x,1),10) > tfc::quantile_T4_Median[a,7]}
  if(method == "T5_ML 0.025")     {d <-T5(standardisiert(x,2),0.025) > tfc::quantile_T5_ML[a,1]}
  if(method == "T5_ML 0.1")        {d <-T5(standardisiert(x,2),0.1) > tfc::quantile_T5_ML[a,2]}
  if(method == "T5_ML 0.5")        {d <-T5(standardisiert(x,2),0.5) > tfc::quantile_T5_ML[a,3]}
  if(method == "T5_ML 2.5")          {d <-T5(standardisiert(x,2),2.5) > tfc::quantile_T5_ML[a,4]}
  if(method == "T5_ML 5")        {d <-T5(standardisiert(x,2),5) > tfc::quantile_T5_ML[a,5]}
  if(method == "T5_ML 10")          {d <-T5(standardisiert(x,2),10) > tfc::quantile_T5_ML[a,6]}
  if(method == "T5_ML 15")         {d <-T5(standardisiert(x,2),15) > tfc::quantile_T5_ML[a,7]}
  if(method == "T5_Median 0.025")     {d <-T5(standardisiert(x,1),0.025) > tfc::quantile_T5_Median[a,1]}
  if(method == "T5_Median 0.1")        {d <-T5(standardisiert(x,1),0.1) > tfc::quantile_T5_Median[a,2]}
  if(method == "T5_Median 0.5")        {d <-T5(standardisiert(x,1),0.5) > tfc::quantile_T5_Median[a,3]}
  if(method == "T5_Median 2.5")          {d <-T5(standardisiert(x,1),2.5) > tfc::quantile_T5_Median[a,4]}
  if(method == "T5_Median 5")        {d <-T5(standardisiert(x,1),5) > tfc::quantile_T5_Median[a,5]}
  if(method == "T5_Median 10")          {d <-T5(standardisiert(x,1),10) > tfc::quantile_T5_Median[a,6]}
  if(method == "T5_Median 15")         {d <-T5(standardisiert(x,1),15) > tfc::quantile_T5_Median[a,7]}
  return(list("Decision" = d))

}

#' @export
cauchy.test <- function(Test , Sample){
  n = length(Sample)
  if (n!=10 & n!= 20 & n!= 50){
     return("Error, wrong sample size.")
  }
  x = c("D_Henze", "KL", "W", "AD", "CM", "KS", "T1", "T2", "T3", "T4")
  if(Test != x[x==Test]){
    return("Error, failure in choosing the test.")
  }
  if (n==10) {a=1}
  if (n==20) {a=2}
  if (n==50) {a=3}
  if( Test == "D_Henze")  {value <- D_Henze(standardisiert(Sample,1),5); d <- value > tfc::quantile_D_Median[a,6]}
  if( Test == "KL")       {value <- D_2(standardisiert(Sample,2)); d <- value > tfc::quantile_KL_ML[a]}
  if( Test == "W")        {value <- W(standardisiert(Sample,2)); d <- value > tfc::quantile_edf_ML[a,4]}
  if( Test == "AD")       {value <- AD(standardisiert(Sample,1)); d <- value > tfc::quantile_edf_Median[a,3]}
  if( Test == "CM")       {value <- CM(standardisiert(Sample,1)); d <- value > tfc::quantile_edf_Median[a,2]}
  if( Test == "KS")       {value <- KS(standardisiert(Sample,1)); d <- value > tfc::quantile_edf_Median[a,1]}
  if( Test == "T2")       {value <- T2(standardisiert(Sample,2),0.025); d <- value > tfc::quantile_T2_ML[a,1]}
  if( Test == "T1")       {value <- T1(standardisiert(Sample,2),5); d <- value > tfc::quantile_T1_ML[a,6]}
  if( Test == "T3")       {value <- T3(standardisiert(Sample,2),10); d <- value > tfc::quantile_T3_ML[a,7]}
  if( Test == "T4")       {value <- T4(standardisiert(Sample,2),10); d <- value > tfc::quantile_T4_ML[a,7]}
  parameters <- median_est(Sample)
  result <- list("test" = Test, "decision" = d, "sample" = Sample, "value" = value, "parameters" = parameters)
  attr(result, "class") <- "tfc"
  result
}

#' @export
print.tfc <- function(obj){
  cat("################################################################################################################## \n")
  cat("         One-sample test for cauchy with the ",obj$test, " teststatistic.\n"  )
  cat("\n")
  cat("data: ", obj$sample, "\n")
  cat(obj$test," = ", obj$value, " \n")
  cat("Estimated location parameter =  ", obj$parameters[1], " \n")
  cat("Estimated scale parameter =  ", obj$parameters[2], " \n")
  if (obj$decision == TRUE) {
    cat("The test has no objection, that the sample is not cauchy distributed under the significane level of 0.095. \n")
  } else{
    cat("The test rejects the assumption, that the sample is cauchy distributed. \n")
  }
  cat("################################################################################################################## \n")
}

