data {
  int<lower=0> n;   // number of options
  int<lower=0> k;   // number of states == number of coefficients
  matrix[n, k] X;   // payoff matrix
  vector[n] y;      // outcome vector
}
parameters {
  simplex[k] b;       // beta coefficients
  real<lower=0> sigma;  // error scale
  real<lower=0> alpha; // parameter for dirichlet distribution
  real<lower=0, upper=1> lambda; // discounting parameter
}
model {
  //lambda ~ uniform(0.9,1);//lambda prior
  alpha ~ weibull(1.1,1); // dirichlet parameter prior
  b ~ dirichlet(rep_vector(alpha, k)); // coef prior
  y ~ normal(lambda * X * b, sigma);  // likelihood
  
}
