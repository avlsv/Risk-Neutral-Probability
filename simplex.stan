//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower=0> n;   // number of data items must match number rows of X
  int<lower=0> k;   // number of predictors must match number columns of X
  matrix[n, k] X;   // predictor matrix. Might include intercept (column of 1s)
  vector[n] y;      // outcome vector
}
parameters {
  simplex[k] b;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
  real<lower=0, upper = 1> alpha; // parameter for dirichlet distribution
}
model {
  alpha ~ beta(2, 2);
  b ~ dirichlet(rep_vector(alpha, k)); // coef prior
  y ~ normal(X * b, sigma);  // likelihood
}
