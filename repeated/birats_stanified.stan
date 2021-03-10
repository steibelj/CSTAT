// http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol2.pdf
// Page 23: Birats
// 

data {
  int<lower=0> N; // Number of rats
  int<lower=0> Npts; // Number of data points
  int<lower=0> rat[Npts]; // Lookup index -> rat
  real x[Npts];
  real y[Npts];
  real xbar;
  cov_matrix[2] Omega; 
}
parameters {
  vector[2] beta[N];
  vector[2] mu_beta;
  real<lower=0> sigmasq_y;
  cov_matrix[2] Sigma_beta; 
}

transformed parameters {
  real<lower=0> sigma_y; 
  sigma_y = sqrt(sigmasq_y); 
} 
model {
  sigmasq_y ~ inv_gamma(0.001, 0.001);
  mu_beta ~ normal(0, 100);
  Sigma_beta ~ inv_wishart(2, Omega); 
  for (n in 1:N) 
    beta[n] ~ multi_normal(mu_beta, Sigma_beta);
    
    for (n in 1:Npts){
    int irat;
    irat = rat[n];
    y[n] ~ normal(beta[irat,1] + beta[irat,2] * x[n], sigma_y);
  }
}
