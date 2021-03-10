data {
  int<lower=0> n;       // Number of years
  int<lower=0> C[n];    // Count
  vector[n] x;       // land type in numeric format 0=grassland 1=arable
}


parameters {
  real<lower=-10,upper=10> alpha;
  real<lower=-10,upper=10> beta;
  real<lower=0> sigma;
  vector[n] e;
}

transformed parameters {
  vector[n] log_lambda;
  vector[n] res;
  res=sigma*e;
  log_lambda = alpha
             + beta * x+res;
}

model {
  // Implicit uniform priors are used for sigma, alpha and beta
  sigma~cauchy(0,5);
  for (i in 1:n)
    e[i]~normal(0,1);
  // Likelihood
  C ~ poisson_log(log_lambda);
  
  
}

generated quantities{s
 int<lower=0> yrep[n];
 vector[n] erep;
 for (i in 1:n)
  erep[i]=normal_rng(0,1);
  
  yrep= poisson_log_rng(alpha+beta * x+erep*sigma); 
}
