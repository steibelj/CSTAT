// https://www.mrc-bsu.cam.ac.uk/wp-content/uploads/WinBUGS_Vol2.pdf
// Page 2: Dugongs 
// 
//
// 


data {
  int<lower=0> N; 
  real x[N]; 
  real Y[N]; 
} 
parameters {
  real alpha; 
  real beta;  
  real<lower=0> sigma2; 
} 

transformed parameters {
  real sigma; 
  sigma = sqrt(sigma2); 
} 

model {
  real m[N];
  for (i in 1:N) 
    m[i] = alpha + beta * x[i];
  Y ~ normal(m, sigma); 
    
  alpha ~ normal(0.0, 1000); 
  beta ~ normal(0.0, 1000); 
  sigma2 ~ scaled_inv_chi_square(3,0.1); 
}

