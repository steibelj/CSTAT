
data {
  int<lower=0> N; 
  real x[N]; 
  real Y[N]; 
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
