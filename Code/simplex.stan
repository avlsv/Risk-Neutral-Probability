data {
  int<lower=0> n;   // number of data items must match number rows of X
  int<lower=0> k;   // number of predictors must match number columns of X
  matrix[n, k] X;   // predictor matrix. Might include intercept (column of 1s)
  vector[n] y;      // outcome vector
}
parameters {
  simplex[k] b;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
  real alpha=1/2; // parameter for dirichlet distribution
  real lambda; // parameter for 
}
model {
  lambda ~ uniform(0.9,1);
  //alpha; // dirichlet parameter prior
  b ~ dirichlet(rep_vector(alpha, k)); // coef prior
  y ~ normal(lambda * X * b, sigma);  // likelihood
  
}
