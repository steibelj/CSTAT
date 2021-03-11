#learning Objective 1: understand how to code a  poisson GLM in stan
#learning Objective 2: understand how overdispersion is modeled in GLM
#learning Objective 3: Perform model checking using ppd

library(rstan)


library(bayesplot)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rm(list=ls())
datadir<-"C:\\Users\\marti\\OneDrive\\Documents\\job\\bayesian inference\\CSTAT\\poisson/"
setwd(datadir)

#Here are represented counts of european hares in parcels of grassland or tillable (arable) land
load("hare.Rdata")
ls()
head(C)
C
head(x)
x

n<-length(C)
n
table(x)
ls()
x.num<-as.numeric(x)-1
data.frame(x,x.num)

boxplot(C~x)
by(C,x,mean)
by(C,x,var)

#It is expected that these counts will follow a Poisson distribution
#the question is if the expected counts will differ significantly between arable land and grassland.
#caution: overdisperssion may be present.

#A simple poisson model is provided (poisson.txt) use the model to:
#1) fit a poisson model, assess convergence
#2) determine if model fit is appropriate using goodness-of-fit statistic included in the code.
#3) modify model as neeed according to fit statistic
#4) answer the question if the expected count differs depending on land use.

data_pois<-list(n=n,C=C,x=x.num)
data_pois
  #invoke winbugs from R, independent heteroskedastic

pois_mod<-stan(file="poisson.stan",data = data_pois,
               chains = 4,warmup = 1000,iter = 2000)
monitor(pois_mod)

print(pois_mod, pars = c("alpha","beta"))
traceplot(pois_mod,pars=c('alpha',"beta"))


#is there a difference in hare numbers between arable and grasasland?
#class proposed some bayesplot graphics

#is beta significant?
#tail area probability?
beta<-extract(pois_mod,pars="beta")
ind_var<-beta$beta<0
tail_area<-2*mean(ind_var)
tail_area


#overdispersion
#notice model change and extra parameter
pois_op_mod<-stan(file="poisson_op.stan",data = data_pois,
                  chains = 4,warmup = 1000,iter = 2000)

yrep<-extract(pois_mod,"yrep")$yrep
yrep2<-extract(pois_op_mod,"yrep")$yrep

dim(yrep)
pp_check(C,yrep,fun="stat",stat="var")
pp_check(C,yrep2,fun="stat",stat="sd",binwidth=0.5)

ppc_dens_overlay(y = C,yrep = yrep[sample(nrow(yrep), 100),])
ppc_dens_overlay(y = C,yrep = yrep2[sample(nrow(yrep2), 100),])

ppc_ecdf_overlay(y = C,yrep = yrep)
ppc_ecdf_overlay(y = C,yrep = yrep2)
