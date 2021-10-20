#Effect of prior

#Learning objective 1: Compare performance and results of regresion model fit with a different prior
#Learning objective 2: Modify code and fit model assuming flat (improper) priors
#learning objective 3: compare results of linear regression fit under 3 different prior assumptions


library(rstan)
library(bayesplot)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rm(list=ls())



setwd("C:/Users/marti/OneDrive/Documents/job/bayesian inference/CSTAT/example 1b/")

#Data
N <- 27  
x <-c(1, 1.5, 1.5, 1.5, 2.5, 4, 5, 5, 7, 8, 8.5, 9, 9.5, 9.5, 10, 
    12, 12, 13, 13, 14.5, 15.5, 15.5, 16.5, 17, 22.5, 29, 31.5)
Y<-c(1.8, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 2.19, 
    2.26, 2.4, 2.39, 2.41, 2.5, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
    2.47, 2.64, 2.56, 2.7, 2.72, 2.57)

#notice how the transformation is included here.
data_reg<-list(N=N, x=log(x),Y=Y)
data_reg



#fit a model
stdug2<-stan(file = "dugongs_linear_2.stan",
            pars=c("alpha","beta","sigma"),
            data = data_reg,
            warmup = 100,
            iter=1000,
            chains = 4)

save(stdug2,file="second_fit.RData" )
load("../example 1/first_fit.RData")
#load("second_fit.RData")

#look into the stanfit object in more detail
pairs(stdug2)
pairs(stdug)

#time elapsed in the MC runs (it does not include compilation time)
get_elapsed_time(stdug2)
get_elapsed_time(stdug)



#summaries
print(stdug2)

#first quick convergence check
monitor(stdug2)


# another summary
sum_met<-summary(stdug2)
sum_met$summary

#perform shinystanas needed
launch_shinystan(stdug2)


#IN CLASS, compare:
stdug_flat<-stan(file = "dugongs_linear_flat.stan",
             pars=c("alpha","beta","sigma"),
             data = data_reg,
             warmup = 100,
             iter=1000,
             chains = 4)

monitor(stdug_flat)
sum_met<-summary(stdug_flat)
sum_met$summary

get_elapsed_time(stdug_flat)

mcmc_acf(stdug_flat)

