quantile_D <- function(reps = 10000,statistic=D,standard = 2) {
  a = c(0.025,0.1,0.5,1.0,2.5,5,10)
  b = c(10,20,50)
  A = matrix(data=NA,nrow=length(b),ncol = length(a))
  pb <- txtProgressBar(title="Example progress bar", label="0% done", min=0, max=100, initial=0,style = 3)
  for (i in 1:length(b)) {
    emp <- apply(replicate(reps,rcauchy(b[i],0,1)),2,standardisiert,method_estimation = standard)
    A[i,] <- sapply(a, function(x) unname(quantile(apply(emp,2,statistic,l=x),0.95)))
    setTxtProgressBar(pb,i/length(b)*100)
  }
  colnames(A) <- a
  rownames(A)<- b
  A
}
quantile_D_Parallel <- function(reps = 10000,statistic=D,standard = 2,cores) {
  a = c(0.025,0.1,0.5,1.0,2.5,5,10)
  b = c(10,20,50)
  #A = matrix(data=do.call(rbind,mclapply(b,f,mc.cores =cores)),nrow=length(b),ncol = length(a))
  f <- function(y){
    emp <- apply(replicate(reps,rcauchy(y,0,1)),2,standardisiert,method_estimation = standard)
    do.call(cbind, parallel::mclapply(a, function(x) unname(quantile(apply(emp,2,statistic,l=x),0.95)),mc.cores=cores))
  }
  A <- matrix(data=do.call(rbind, parallel::mclapply(b,f,mc.cores =cores)),nrow=length(b),ncol = length(a))
  colnames(A) <- a
  rownames(A)<- b
  A
}
quantile_T5 <- function(reps = 10000,standard = 2,cores) {#1.0 darf nicht als Parameter gewählt werden
  a = c(0.025,0.1,0.5,2.5,5,10,15)
  b = c(10,20,50)
  #A = matrix(data=do.call(rbind,mclapply(b,f,mc.cores =cores)),nrow=length(b),ncol = length(a))
  f <- function(y){
    emp <- apply(replicate(reps,rcauchy(y,0,1)),2,standardisiert,method_estimation = standard)
    do.call(cbind,parallel::mclapply(a, function(x) unname(quantile(apply(emp,2,T5,l=x),0.95)),mc.cores=cores))
  }
  A <- matrix(data=do.call(rbind, parallel::mclapply(b,f,mc.cores =cores)),nrow=length(b),ncol = length(a))
  colnames(A) <- a
  rownames(A)<- b
  A
}
quantile_Schaubild_Param <- function(reps = 10000,statistic=D,standard = 2,cores) {
  a = seq(0.1,10.1,0.2)
  b = 20
  f <- function(y){
    emp <- apply(replicate(reps,rcauchy(y,0,1)),2,standardisiert,method_estimation = standard)
    do.call(cbind,parallel::mclapply(a, function(x) unname(quantile(apply(emp,2,statistic,l=x),0.95)),mc.cores=cores))
  }
  A <- matrix(data=f(b),nrow=length(b),ncol=length(a))
  colnames(A) <- a
  rownames(A)<- b
  A
}

quantile_edf <- function(reps=10000,standard = 2) {
  A = matrix(data=NA,nrow=5,ncol = 4)
  for (n in c(10,20,50,100,200)) {
    emp <- apply(replicate(reps,rcauchy(n,0,1)),2,standardisiert,method_estimation = standard)
    KS_Statistik = apply(emp , 2, KS)
    CM_Statistik = apply(emp , 2, CM)
    AD_Statistik = apply(emp , 2, AD)
    W_Statistik = apply(emp , 2, W)
    if (n==10) {a=1}
    if (n==20) {a=2}
    if (n==50) {a=3}
    if (n==100) {a=4}
    if (n==200) {a=5}
    A[a,]=c(quantile(KS_Statistik,0.95),quantile(CM_Statistik,0.95), quantile(AD_Statistik,0.95),quantile(W_Statistik,0.95))
  }
  colnames(A) <- c("KS","CM","AD","W")
  rownames(A)<- c("10","20","50","100","200")
  A
}
quantile_KL <- function(reps=10000,standard = 2) {
  A = matrix(data=NA,nrow=5,ncol = 1)
  pb <- txtProgressBar(title="Example progress bar", label="0% done", min=0, max=100, initial=0,style = 3)
  for (n in c(10,20,50,100)) {
    emp <- apply(replicate(reps,rcauchy(n,0,1)),2,standardisiert,method_estimation = standard)
    KL_Statistik = apply(emp , 2, KL)
    if (n==10) {a=1}
    if (n==20) {a=2}
    if (n==50) {a=3}
    if (n==100) {a=4}
    if (n==200) {a=5}
    A[a,1]=c(quantile(KL_Statistik,0.95))
    setTxtProgressBar(pb,a/5*100)
  }
  colnames(A) <- c("KL")
  rownames(A)<- c("10","20","50","100","200")
  A
}
