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
  real<lower=0> sigma; 
} 


model {
  real m[N];
  mateff[1]~normal(0.0,0.00001);
  
  for (i in 1:N) 
    m[i] = sexeff[sex[i]]+mateff[mating[i]];
  Y ~ normal(m, sigma); 
}


generated quantities{
  real diff;
  real S1;
  real S2;
  real <lower=0> vare;
  vare= sigma^2;
  S1=sexeff[1]+mean(mateff);
  S2=sexeff[2]+mean(mateff);
  diff = sexeff[2]-sexeff[1];
}

