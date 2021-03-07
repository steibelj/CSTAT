#First program in Stan

#Learning objective 1: Understand the basics of stan syntax
#Learning objective 2: Fit a simple regression model using stan
#Learning objective 3: Undertand the structure of a stanfit object and how to extract information from it
#Learning objective 4: Perform convergence diagnostics and sumaries with stan, bayesplot and shinystan

library(rstan)
library(bayesplot)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rm(list=ls())



setwd("C:/Users/marti/OneDrive/Documents/job/bayesian inference/CSTAT")

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


#fit frequentist model
lmf<-lm(Y~x,data=data_reg) #notice the data!!!
#summary
smf<-summary(lmf)
smf
#residual variance
(smf$sigma)^2

prc<-predict(lmf, interval="confidence" )


plot(Y~x,ylim=c(1.5,3),xlab="age",ylab="length",pch=19)
points(prc[,1]~x,type="l",lwd=2)


#now let's complete the Bayesian Regression Analysis 

#fit a model
stdug<-stan(file = "dugongs_linear.stan",
            pars=c("alpha","beta","sigma"),
            data = data_reg,
            warmup = 100,
            iter=1000,
            chains = 4)
class(stdug)
str(stdug)
save(stdug,file="first_fit.RData" )
#load("first_fit.RData")
#look into the stanfit object in more detail

#sampler parameters
smpar<-get_sampler_params(stdug,inc_warmup=F)

str(smpar)
head(smpar[[1]])
#mean acceptance rate
sapply(smpar,function(x) mean(x[,"accept_stat__"]))
#max stepsize
sapply(smpar,function(x) max(x[,"stepsize__"]))
#are there divergent chains?
sapply(smpar,function(x) sum(x[,"divergent__"]>0))


#iterations
#extract as matrix or data frame
iterc<-extract(stdug)
class(iterc)
head(as.matrix(iterc))
head(as.data.frame(iterc))
mcmc_sample<-as.data.frame(iterc)
dim(mcmc_sample)
colMeans(mcmc_sample)
apply(mcmc_sample,2,sd)
smf


#the code is contained in the stanfit object
get_stancode(stdug)
code_reg<-get_stancode(stdug)
print(code_reg)
cat(code_reg)

# initial values
get_inits(stdug)

#randon number generator seed
get_seed(stdug)

#time elapsed in the MC runs (it does not include compilation time)
get_elapsed_time(stdug)



#methods for stanfit objects
#graphics
plot(stdug)
plot(stdug,pars=c("beta","sigma"))
traceplot(stdug)

#summaries
print(stdug)

#first quick convergence check
monitor(stdug)


# another summary
sum_met<-summary(stdug)
str(sum_met)
sum_met$summary
#per chanin
sum_met$c_summary

#package bayesplot
#diagnostics
mcmc_trace(stdug)
mcmc_scatter(stdug,pars=c("alpha","beta"))
mcmc_pairs(stdug)
mcmc_acf(stdug)
mcmc_acf_bar(stdug)

#graphical summaries
mcmc_intervals(stdug)
mcmc_intervals(stdug,pars=c("beta","sigma"),prob = 0.9,prob_outer = 0.99)

mcmc_areas(mcmc_sample,pars = c("alpha","beta","sigma"))

plot_title<-ggtitle("Posterior distributions","median and 80% posterior prob. interval")
mcmc_areas(mcmc_sample,pars = c("beta","sigma"),prob=0.9,prob_outer = 0.99)+plot_title

mcmc_hist(mcmc_sample)


#shinystan
launch_shinystan(stdug)
