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
  real mu;
  real<lower=0> sigma; 
} 


model {
  real m[N];
  sexeff[1]~normal(0.0,0.0001); // restricting first level of sex effect to be 0.0
  
  for (i in 1:N) 
    m[i] = mu+sexeff[sex[i]];
    
  Y ~ normal(m, sigma); 
}

generated quantities{
  real S1;
  real S2;
  real diff;
  real <lower=0> vare;
  S1= mu + sexeff[1];
  S2= mu + sexeff[2];  
  vare= sigma^2;
  diff = sexeff[2]-sexeff[1];
}
