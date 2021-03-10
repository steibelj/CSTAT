#prediction

#Learning objective 1: Identify in a stanmodel how parameter transformations are generated
#Learning objective 2: Use generated quantities to perform inferences
#learning objective 3: Use R coding to generate parameter transformations


library(rstan)
library(bayesplot)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rm(list=ls())



setwd("C:/Users/marti/OneDrive/Documents/job/bayesian inference/CSTAT/")

#Data
N <- 27  
x <-c(1, 1.5, 1.5, 1.5, 2.5, 4, 5, 5, 7, 8, 8.5, 9, 9.5, 9.5, 10, 
    12, 12, 13, 13, 14.5, 15.5, 15.5, 16.5, 17, 22.5, 29, 31.5)
Y<-c(1.8, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 2.19, 
    2.26, 2.4, 2.39, 2.41, 2.5, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
    2.47, 2.64, 2.56, 2.7, 2.72, 2.57)
x0<-10
#notice how the transformation is included here.
data_reg<-list(N=N, x=log(x),Y=Y,x0=log(x0))
data_reg



#fit a model
stdug<-stan(file = "dugongs_linear_check.stan",
            pars=c("alpha","beta","sigma","m0","y0"),
            data = data_reg,
            warmup = 100,
            iter=20000,
            thin=4,
            chains = 1)

save(stdug,file="regress_check.RData")
load("regress_check.RData")


monitor(stdug)
sum_met<-summary(stdug)
sum_met$summary

plot(stdug,pars=c("m0","y0"),prob=0.95,outer.prob=0.99)

#frequentist analysis for comparison
#fit frequentist model
lmf<-lm(Y~x,data=data_reg)
#summary
smf<-summary(lmf)
smf

prc<-predict(lmf, interval="confidence" )

prp<-predict(lmf, interval="prediction" )

prc[x==10.]
prp[x==10.]
summary(stdug,pars=c("m0","y0"))


plot(Y~x,ylim=c(1.5,3),xlab="age",ylab="length",pch=19)
points(prc[,1]~x,type="l",lwd=2)
points(prc[,2]~x,type="l",col="red",lty="dashed")
points(prc[,3]~x,type="l",col="red",lty="dashed")

points(prp[,2]~x,type="l",col="green",lty="dotted")
points(prp[,3]~x,type="l",col="green",lty="dotted")
abline(v=10)


#generate quanties from R

mcmc_sample<-as.data.frame(extract(stdug,pars=c("alpha","beta","sigma")))

head(mcmc_sample)

expected_value<-t(apply(mcmc_sample, 1,function(w) w[1]+w[2]*log(x)))
dim(expected_value)
head(expected_value)

ypred=colMeans(expected_value)
points(x,ypred,col="red",lwd=2,type="l",lty="dashed")

cll<-apply(expected_value,2,quantile,probs=0.025)
clu<-apply(expected_value,2,quantile,probs=0.975)
points(x,cll)
points(x,clu)

#prediction interval
e<-t(apply(mcmc_sample,1,function(w) rnorm(n = N)*w[3]))
dim(e)
y_rep<-expected_value+e

pll<-apply(y_rep,2,quantile,probs=0.025)
plu<-apply(y_rep,2,quantile,probs=0.975)
points(x,pll,col="blue")
points(x,plu,col="blue")

#now let's use quantities extracted from stanfit object
expected_value<-as.data.frame(extract(stdug,pars="y0"))
hist(expected_value$y0)
abline(v=Y[x==10],lwd=5)

       