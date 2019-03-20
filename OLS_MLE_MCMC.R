######################################################
#######Maximum likelihood estimation (MLE)##############
#######################################################

loglik=function(para){
  N=length(totalexp)
  e=foodexp-para[1]-para[2]*totalexp
  ll=-0.5*N*log(2*pi)-0.5*N*log(para[3]^2)-0.5*sum(e^2/para[3]^2)
  return(ll)
}

library(maxLik)
res=maxLik(loglik,start=c(0.1,1,1))
summary(res)

lm.food=lm(foodexp~totalexp,food) #OLS linear regression
summary(lm.food)
plot(lm.food)
par(mfrow=c(2,2))

lm.food2=lm(foodexp~-1+totalexp, food) #No intercept regression
summary(lm.food2)

sfoodexp=scale(foodexp) #scale variable
stotalexp=scale(totalexp)

lm.sfood=m(sfoodexp~stotalexp) # linear regression with scaled variables
summary(lm.sfood)
coef(lm.food)
resid(lm.food)
predict(lm.food)
predict(lm.food, data.frame(totalexp=c(900,1000))) 

conf=predict(lm.food, data.frame(totalexp=c(900,1000)),interval="confidence")
pred=predict(lm.food, data.frame(totalexp,interval="prediction") 
             plot(totalexp,foodexp)
             abline(lm.food)
             lines(totalexp,conf[,2])
             lines(totalexp, conf[,3]) # 95% confidence level
             lines(totalexp,pred[,2],lty=3)
             lines(totalexp, pred[,3],lty=3) # prediction for sample
             
             ###########MCMC
             beta0=1:1000
             beta1=1:1000
             b1=20; b2=0.6; n=25
             x=10:34
             
             for (1 in 1:1000){
               u=rnorm(25)
               y=b1+b2*x+u
               lm.m=lm(y~x)
               beta0[i]=as.numeric(coef(lm.m)[1]])
               beta1[i]=as.numeric(coef(lm.m)[2]])
             }
             mean(beta0)
             mean(beta1)