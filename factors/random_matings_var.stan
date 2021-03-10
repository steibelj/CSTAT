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
  real <lower=0> varm;
  real<lower=0> vare; 
} 


model {
  real m[N];
  for (j in 1:nmat)
    mateff[j]~normal(0.0,sqrt(varm));
    
  for (i in 1:N) 
    m[i] = sexeff[sex[i]]+mateff[mating[i]];
  Y ~ normal(m, sqrt(vare)); 
}

generated quantities{
  real diff;
  diff=sexeff[2]-sexeff[1];
}
