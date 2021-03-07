
data {
  int<lower=0> N; 
  real x[N]; 
  real Y[N]; 
} 
parameters {
  real alpha; 
  real beta;  
  real<lower=0> tau; 
} 

transformed parameters {
  real sigma; 
  sigma = 1 / sqrt(tau); 
} 

model {
  real m[N];
  for (i in 1:N) 
    m[i] = alpha + beta * x[i];
  Y ~ normal(m, sigma); 
    
  alpha ~ normal(0.0, 1000); 
  beta ~ normal(0.0, 1000); 
  tau ~ gamma(.0001, .0001); 
}
