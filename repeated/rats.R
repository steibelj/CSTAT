# BUGS rats example (Vol 1, Example 1)
# http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol1.pdf
#Example modified by JPS from Stan github

library(rstan)
library(bayesplot)
library(ggplot2)
library(gridExtra)
rstan_options(auto_write = TRUE)

options(mc.cores = parallel::detectCores(logical = FALSE))

chains = 4
iter = 1000

workdir<-datadir<-"C:\\Users\\marti\\OneDrive\\Documents\\job\\bayesian inference\\CSTAT\\repeated/"
setwd(datadir)

sourceToList = function(file){
  source(file, local = TRUE)
  d = mget(ls())
  d$file = NULL
  d
}

# Data are the same for all models
data = sourceToList("rats.data.R")
#data and descriptive analyses
data


head(data$y)
plot(0,0,xlim=c(min(data$x),max(data$x)),ylim=c(0,max(data$y)),xlab="age",ylab="weight")
for (i in 1:nrow(data$y)){
  points(data$y[i,]~data$x,type="l")
}     



# Indexed data for use with rats_stanified.stan
dataIndexed = with(data, list(
  N = N,
  Npts = length(y),
  rat = rep(1:nrow(y), ncol(y)),
  x = rep(x, each = nrow(y)),
  y = as.numeric(y),
  xbar = xbar
))

dataIndexed
to_plot<-data.frame(age=dataIndexed$x,weight=dataIndexed$y,animal=as.factor(dataIndexed$rat))


p1<-ggplot(to_plot,aes(x=age,y=weight))+geom_point()+geom_smooth(method="lm")
p2<-ggplot(to_plot,aes(x=age,y=weight,color=animal))+geom_point()+geom_smooth(method="lm")

grid.arrange(p1, p2, nrow = 2)

# With random init
chains = 4
iter = 1000

rats = stan(file = "rats.stan", data = data, chains = chains,  iter = iter)
monitor(rats)

print(rats,pars=c("sigma_alpha","sigma_beta","sigma_y","mu_alpha","mu_beta"))
mcmc_acf(rats,pars=c("sigma_alpha","sigma_beta","sigma_y","mu_alpha","mu_beta"))

mcmc_intervals(rats,pars=c("sigma_alpha","sigma_beta","sigma_y"))
mcmc_intervals(rats,pars=vars(param_range("beta",1:30)))
mcmc_intervals(rats,pars=vars(param_range("alpha",1:30)))


# Indexed and simplified rats example, using random initialization
rats_stanified = stan(file = "rats_stanified.stan", data = dataIndexed, chains = chains,
                      iter = iter)
print(rats_stanified,pars=c("sigma_alpha","sigma_beta","sigma_y","mu_alpha","mu_beta"))



datain2<-dataIndexed
datain2$Omega<-structure(c(1, 0, 0, 1), .Dim = c(2, 2)) 
datain2

rats_rcm = stan(file = "birats_stanified.stan", data = datain2, chains = chains,
                      iter = iter)
#monitor(rats_rcm)
print(rats_rcm,pars=c("Sigma_beta","sigma_y","mu_beta"))
  
#class/home challenge1:
#Propose ideas for:
#extract inferences on the growth curve
#compare the results obtained by each model

#class/home challenge2: 
#use rstanarm to fit these models
#which priors are used by default?
#compare effective sample size
#compare posteriod distributions


library(rstanarm)
data_st<-data.frame(y=dataIndexed$y,x=dataIndexed$x,rat=dataIndexed$rat)
ratsbi<-stan_lmer(y~x+(1+x|rat),data=data_st)
monitor(ratsbi$stanfit)
prior_summary(ratsbi)

summary(ratsbi)
ratsbi$covmat
