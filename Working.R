library(ggplot2)
library(glmnet)
library(MASS)

elastic.net.est<-function(n, X, Y, alpha){
  fit<-cv.glmnet(X, Y, family="gaussian", alpha=alpha)
  lambda<-fit$lambda.min
  coefs<-coef(fit, s="lambda.min")
  print(coefs)
  signif.pred<-length(which(coefs!=0))
  yhat<-cbind(0, X)%*%coefs
  SE<-mean((Y-yhat)^2)
  output<-cbind(alpha, lambda, signif.pred, SE)
  return(output)
}

simulate.elastic.net<-function(n, p, d, rho, beta, alpha){
  A<-diag(1, d, d)
  for(i in 1:d){
    for(j in 1:d){
      if(j!=i){A[i,j]<-rho}}}
  Cov<-diag(1, p, p)
  for(i in 1:d){
    for( j in 1:d){
      Cov[i,j]<-A[i,j]}}
  X<-mvrnorm(n, rep(0, p), Cov)
  Y<-X%*%beta+rnorm(n)
  mod<-elastic.net.est(n, X, Y, alpha)
  return(mod)}

MonteCarlo.elastic.net<-function(n, p, d, rho, beta, alpha, K){
  output<-cbind(1:K, 0, 0, 0, 0)
  for(k in 1:K){
    mod<-simulate.elastic.net(n, p, d, rho, beta, alpha)
    output[k,(2:5)]<-mod
  }
  return(output)
}

vector.MonteCarlo.En<-function(n, p, d, Rho, beta, Alpha, K){
  results<-expand.grid(Alpha, Rho)
  results<-cbind(results, 0, 0, 0)
  colnames(results)<-c("Alpha", "Rho", "Lambda", "No. Variables", "MSE")
  for(i in 1:length(results[,1])){
    mod<-MonteCarlo.elastic.net(n, p, d, results[i,2], beta, results[i,1], K)
    results[i, 3]<-mean(mod[,3])
    results[i, 4]<-mean(mod[,4])
    results[i, 5]<-mean(mod[,5])
  }
  return(results)
  
}



n<-100 #number of observations
beta<-c(1,1,2) #true values of beta
rho<-0.99 #values of cov(X_1, X_2)
alpha<-c(0, 0.3, 0.7, 1) 

mod1<-vector.MonteCarlo.En(n, p=3, d=2, rho, beta, alpha,1)
ggplot(mod1, aes(x=))



#Now suppose n<p
#Suppose we have a group of d predictor variables that are correlated, i.e. they all have correlation rho.
n<-50
p<-100
d<-5
rho<-0.75
beta<-c(rep(1, d), runif(p-d, -2, 2))
alpha<-0.9

mod2<-simulate.elastic.net(n, p, d, rho, beta, alpha)  

mod3<-MonteCarlo.elastic.net(n, p, d, rho, beta, alpha, K)



K<-10
Alpha<-c(0.1, 0.3, 0.9)
Rho<-seq(0, 0.9, 0.25)

mod4<-vector.MonteCarlo.En(n, p, d, Rho, beta, Alpha, K)




