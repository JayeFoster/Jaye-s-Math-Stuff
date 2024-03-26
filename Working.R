library(ggplot2)
library(glmnet)
library(MASS)


n<-100 #p=2
beta<-c(1,2,3)
rho<- c(0, 0.9)
alpha<-c(0,0.5)
K<-1

elastic_net2<-function(n, beta, rho, alpha, K){
  num_alpha<-length(alpha)
  num_rho<-length(rho)
  Rho<-rep(0, num_alpha*num_rho)
  Alpha<-rep(0, num_alpha*num_rho)
  mse<-matrix(0, num_rho*num_alpha, K)
  MSE_K<-cbind(Rho, Alpha, mse)
  
  for(k in 1:K){
    row=1
    for(j in 1:num_rho){
      cov<-matrix(data = c(1, rho[j], rho[j], 1), nrow=2, ncol=2)
      X<-mvrnorm(n, mu=c(0,0), Sigma =cov)
      design<-cbind(rep(1,n), X )
      Y<-design%*%beta+rnorm(n)
      for(i in 1:num_alpha){
        fit_i<-cv.glmnet(X, Y, family="gaussian", alpha=alpha[i])
        fit_i_coef<-coef(fit_i, s="lambda.min")
        print(fit_i_coef)
        yhat<-design%*%fit_i_coef
        if(k<=1){
          MSE_K[row,1]<-rho[j]
          MSE_K[row,2]<-alpha[i]
        }
        print(mean((Y-yhat)^2))
        MSE_K[row,(k+2)]<-mean((Y-yhat)^2)
        row=row+1
      }}}
  results<-cbind(MSE_K[,(1:2)], 0)
  if(K==1){
    results[,3]<-MSE_K[,3]
  }
  else{
    results[,3]<-rowMeans(MSE_K[,(3:K+2)])
  }
  colnames(results)[3]<-"MSE"
  
  
  
  return(as.data.frame(results))
}

A<-elastic_net2(n, beta, rho, alpha, 4)
ggplot(data=A, aes(x=Rho, y=MSE, group=Alpha))+geom_line(aes(colour=Alpha))