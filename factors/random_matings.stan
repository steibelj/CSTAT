// Matings:randomized complete blocks.  
// 


data {
  int<lower=0> N;
  int<lower=0> nsex;
  int<lower=0> nmat;
  int<lower=0> mating[N];
  int<lower=0> sex[N];
  real Y[N]; 
} 

parameters {
  real sexeff[nsex]; 
  real mateff[nmat];
  real <lower=0> sigma_m;
  real<lower=0> sigma; 
} 


model {
  real m[N];
  for (j in 1:nmat)
    mateff[j]~normal(0.0,sigma_m);
    
  for (i in 1:N) 
    m[i] = sexeff[sex[i]]+mateff[mating[i]];
  Y ~ normal(m, sigma); 
}

generated quantities{
  real <lower=0> vare;
  real <lower=0> varm;
  real diff;
  vare= sigma^2;
  varm= sigma_m^2;
  diff=sexeff[2]-sexeff[1];
}
