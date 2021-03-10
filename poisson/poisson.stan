data {
  int<lower=0> n;       // Number of years
  int<lower=0> C[n];    // Count
  vector[n] x;       // land type in numeric format 0=grassland 1=arable
}


parameters {
  real<lower=-10,upper=10> alpha;
  real<lower=-10,upper=10> beta;
}

transformed parameters {
  vector[n] log_lambda;

  log_lambda = alpha
             + beta * x;
}

model {
  // Implicit uniform priors are used.

  // Likelihood
  C ~ poisson_log(log_lambda);
}

generated quantities{
 int<lower=0> yrep[n];
  yrep= poisson_log_rng(log_lambda); 
}
