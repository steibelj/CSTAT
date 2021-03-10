#fitting linear classification effects models
#learning objective 1: Understand coding of classification effects linear models, alternative parametrizations
#learning objective 2: Understand modeling of more than one factor (RCBM) and "random" vs "fixed effects"
#NOTE: When we treat a factor as random, is the first multi-level model we use in class
#learning objective 3: Modify code and compare effect of prior

library(rstan)
library(shinystan)
library(bayesplot)
library(lme4)
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



setwd("C:/Users/marti/OneDrive/Documents/job/bayesian inference/CSTAT/factors/")



data_sex<-list(Y=c(13.4, 7.6, 14.5, 11.4, 11.6, 10.5, 12.6, 11.7, 15.3,13.2, 17.5, 16.2, 14.8, 10.7, 12.6, 11.9, 9.1, 11.2, 13.1, 10.5),
                   sex=c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
                   nsex=2,
                   N=20)
data_sex

#frequentist result

lmo<-lm(Y~as.factor(sex),data=data_sex)
summary(lmo)
lsmeans::lsmeans(lmo,"sex")

#Bayesian model
stan_anova<-stan(file="matings.stan",data=data_sex,warmup = 1000,thin = 1, chains = 4, iter = 2000)
#check for warnings....
#save(stan_anova,file="sex_only.RData")


monitor(stan_anova)
traceplot(stan_anova)
summary(stan_anova,pars=c('S1',"S2","diff","vare"))$summary



mcmc_intervals(stan_anova,pars=c("diff","vare"),prob = 0.95,prob_outer = 0.99)

plot_title<-ggtitle("Posterior distributions","difference and mean by sex")
mcmc_areas(stan_anova,pars = c("diff","S1","S2"),prob=0.95,prob_outer = 0.99)+plot_title

#cell mean model
stan_cell_m<-stan(file="cell_mean.stan",data=data_sex,warmup = 1000,thin = 1, chains = 4, iter = 2000)
#save(stan_cell_m,file="cell_mean.RData")

monitor(stan_cell_m)
traceplot(stan_cell_m)
summary(stan_cell_m,pars=c("sexeff","diff","vare"))$summary

plot_title<-ggtitle("Posterior distributions","difference and mean by sex")
mcmc_areas(stan_cell_m,pars = c("sexeff[1]","sexeff[2]","diff","vare"),prob=0.95,prob_outer = 0.99)+plot_title




data_matings<-list(Y=c(13.4, 7.6, 14.5, 11.4, 11.6, 10.5, 12.6, 11.7, 15.3,13.2, 17.5, 16.2, 14.8, 10.7, 12.6, 11.9, 9.1, 11.2, 13.1, 10.5),
                   sex=c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
                   mating=c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10),
                   nsex=2,
                   nmat=10,
                   N=20)
data_matings



lmo<-lm(Y~as.factor(sex)+as.factor(mating),data=data_matings)
summary(lmo)
lsmeans::lsmeans(lmo,"sex")
anova(lmo)

stan_fix0<-stan(file="fixed_matings.stan",data=data_matings,warmup = 2000,thin = 5, chains = 4, iter = 6000)
#save(stan_fix0,file="fixed_mating.RData")
#check for warnings....
pairs(stan_fix0,pars=c("mateff","diff","S1","S2","vare"),include=F)

monitor(stan_fix0)
sm0<-summary(stan_fix0,pars=c("S1","S2","diff","vare"))
sm0$summary

plot_title<-ggtitle("Posterior distributions","difference and mean by sex")
mcmc_areas(stan_fix0,pars = c("diff","vare"),prob=0.95,prob_outer = 0.99)+plot_title



#an idea to deal with the warning
stan_fix<-stan(file="fixed_matings.stan",data=data_matings,warmup = 2000,thin = 5, chains = 4, iter = 6000,
               control = list(max_treedepth = 15))
monitor(stan_fix)
traceplot(stan_fix,pars=c("sexeff","diff","S1","S2"))
pairs(stan_fix,pars=c("mateff","diff","vare"),include=F)


summary(stan_fix)$summary


lmr<-lmer(Y~as.factor(sex)+(1|mating),data=data_matings)
summary(lmr)
lsmeans::lsmeans(lmr,"sex") 


stan_ran<-stan(file="random_matings.stan",pars=c("diff","vare","varm","sexeff"),data=data_matings,warmup = 3000,thin = 5, chains = 4, iter = 10000)
#save(stan_ran,file="ran_mating.RData")
pairs(stan_ran)

monitor(stan_ran)
traceplot(stan_ran)

summary(stan_ran,pars=c("diff","vare","sexeff","varm"))$summary


plot_title<-ggtitle("Posterior distributions","difference and mean by sex")
mcmc_areas(stan_ran,pars = c("diff","vare","varm"),prob=0.95,prob_outer = 0.99)+plot_title




# a model with an alternative parameterization: Flat prior on variances, not on SD
stan_ran2<-stan(file="random_matings_var.stan",data=data_matings,warmup = 3000,thin = 5, chains = 4, iter = 10000)
pairs(stan_ran2,pars=c("mateff","diff","S1","S2"),include=F)
#save(stan_ran2,file="ran_mating2.RData")

monitor(stan_ran2)
summary(stan_ran,pars=c("diff","vare","sexeff","varm"))$summary
summary(stan_ran2,pars=c("diff","vare","sexeff","varm"))$summary

