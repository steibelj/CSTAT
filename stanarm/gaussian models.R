#rstanarm

#Learning objective 1: Understand the syntaxis of rstanarm
#Learning objective 2: Use rstanarm to fit regression and classification effects linea rmodel
#Learning objective 3: Use rstanarm and bayesplot for posterior predictive model checking

library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())
library(rstanarm)
options(mc.cores = parallel::detectCores())
rm(list=ls())



setwd("C:/Users/marti/OneDrive/Documents/job/bayesian inference/CSTAT/stanarm/")

#Data
x <-c(1, 1.5, 1.5, 1.5, 2.5, 4, 5, 5, 7, 8, 8.5, 9, 9.5, 9.5, 10, 
    12, 12, 13, 13, 14.5, 15.5, 15.5, 16.5, 17, 22.5, 29, 31.5)
Y<-c(1.8, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 2.19, 
    2.26, 2.4, 2.39, 2.41, 2.5, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
    2.47, 2.64, 2.56, 2.7, 2.72, 2.57)

#notice how the transformation is included here.
data_reg<-data.frame(x=log(x),Y=Y)
data_reg


reg1<-stan_glm(Y~x,
               data=data_reg,
               family = gaussian)


class(reg1)
names(reg1)
reg1$stanfit
rstan::monitor(reg2$stanfit)

prior_summary(reg1)

reg1
summary(reg1)
rstan::summary(reg1$stanfit)

#using flat priors:
reg2<-stan_glm(Y~x,
               prior=NULL,
               prior_aux = NULL,
               prior_intercept = NULL,
               data=data_reg,
               family = gaussian)


prior_summary(reg2)
summary(reg2)

#Diagnostics
launch_shinystan(reg1)


#posterior predictive checks using bayesplot
pp_check(reg1)

pp_check(reg1,plotfun = "hist",nreps = 5)
pp_check(reg1,plotfun = "stat",stat="mean")
pp_check(reg1,plotfun = "stat",stat="min")
pp_check(reg1,plotfun = "stat",stat="sd")
pp_check(reg1,plotfun = "stat_2d",stat=c("mean","sd"))
xu<-unique(x)

yrep<-posterior_predict(reg1,newdata = data.frame(x=log(xu)))
class(yrep)
boxplot(t(yrep)~xu,at=xu)
points(Y~x,pch=20,col="red")


#Mixed linear model
data_matings<-list(Y=c(13.4, 7.6, 14.5, 11.4, 11.6, 10.5, 12.6, 11.7, 15.3,13.2, 17.5, 16.2, 14.8, 10.7, 12.6, 11.9, 9.1, 11.2, 13.1, 10.5),
                   sex=c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
                   mating=as.factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)))
data_matings<-as.data.frame(data_matings)
head(data_matings)

matmod<-stan_lmer(Y~sex+(1|mating),data=data_matings)
prior_summary(matmod)

coef(matmod)
posterior_interval(matmod)

rstan::summary(matmod$stanfit)$summary[,1]


#challenge: can you specify flat priors for all parameters?


