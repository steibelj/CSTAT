
data {
  int<lower=0> N; 
  real x[N]; 
  real Y[N]; 
  real x0;
} 
parameters {
  real alpha; 
  real beta;  
  real<lower=0> sigma; 
} 

model {
  real m[N];
  for (i in 1:N) 
    m[i] = alpha + beta * x[i];
  Y ~ normal(m, sigma); 
}

generated quantities{
  real m0;
  real y0;
  real e0;
  m0=alpha+beta*x0;
  e0=normal_rng(0.0,sigma); //this is just an extra residual to compute the credibility interval of a single observation
  y0= m0 + e0;
}
