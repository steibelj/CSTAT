// http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol1.pdf
// Page 3: Rats
data {
  int<lower=0> N;
  int<lower=0> T;
  real x[T];
  real y[N,T];
  real xbar;
}
parameters {
  real alpha[N];
  real beta[N];

  real mu_alpha;
  real mu_beta;          // beta.c in original bugs model

  real<lower=0> sigmasq_y;
  real<lower=0> sigmasq_alpha;
  real<lower=0> sigmasq_beta;
}
transformed parameters {
  real<lower=0> sigma_y;       // sigma in original bugs model
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;

  sigma_y = sqrt(sigmasq_y);
  sigma_alpha = sqrt(sigmasq_alpha);
  sigma_beta = sqrt(sigmasq_beta);
}
model {
  mu_alpha ~ normal(0, 100); //overall intercept
  mu_beta ~ normal(0, 100); //overall slope
  sigmasq_y ~ inv_gamma(0.001, 0.001); //error sd
  sigmasq_alpha ~ inv_gamma(0.001, 0.001); //sintercept sd
  sigmasq_beta ~ inv_gamma(0.001, 0.001); //slope sd
  alpha ~ normal(mu_alpha, sigma_alpha); // vectorized mu_alpha+alpha_d; alpha_d~N(0,sigma_alpha)
  beta ~ normal(mu_beta, sigma_beta);  // vectorized
  for (n in 1:N)
    for (t in 1:T) 
      y[n,t] ~ normal(alpha[n] + beta[n] * (x[t] - xbar), sigma_y);

}

//generated quantities {
//  real alpha0;
//  alpha0 = mu_alpha - xbar * mu_beta;
//}
