// Matings:randomized complete blocks.  
// 


data {
  int<lower=0> N;
  int<lower=0> nsex;
  int<lower=0> sex[N];
  real Y[N]; 
} 
parameters {
  real sexeff[nsex]; 
  real<lower=0> sigma; 
} 


model {
  real m[N];

  for (i in 1:N) 
    m[i] = sexeff[sex[i]];
  Y ~ normal(m, sigma); 
}

generated quantities{

  real diff;
  real <lower=0> vare;
  diff= sexeff[2]-sexeff[1];
  vare= sigma^2;
}
