data {
  int<lower = 1> N; // # of responses
  int<lower = 1> I; // # of actions
  int<lower = 1> J; // # of participants
  int<lower = 1> K; // # of predictors
  int<lower = 1, upper = I> ii[N];
  int<lower = 1, upper = J> jj[N];
  matrix[N, K] X;
  int<lower = 0, upper = 1> y[N];
}
parameters {
  vector[J] theta_z;
  vector[I] logalpha_z;
  vector[I] beta_z;
  real<lower = 0> sigma_theta;
  real<lower = 0> sigma_alpha;
  real<lower = 0> sigma_beta;
  vector[K] delta;
  real mu_beta;
}
transformed parameters {
  vector[J] theta = theta_z * sigma_theta;
  vector<lower = 0>[I] alpha = exp(logalpha_z * sigma_alpha);
  vector[I] beta = mu_beta + beta_z * sigma_beta;
}
model {
  theta_z ~ normal(0, 1);
  logalpha_z ~ normal(0, 1);
  beta_z ~ normal(0, 1);
  sigma_theta ~ cauchy(0, 3);
  sigma_alpha ~ cauchy(0, 3);
  sigma_beta ~ cauchy(0, 3);
  delta ~ normal(0, 3);
  mu_beta ~ normal(0, 3);
  y ~ bernoulli_logit(alpha[ii] .* (theta[jj] - beta[ii] + X * delta));
}
