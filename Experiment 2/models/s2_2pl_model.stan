data {
  int<lower = 1> N; // # of responses
  int<lower = 1> I; // # of actions
  int<lower = 1> J; // # of participants
  int<lower = 1> K; // # of conditions
  int<lower = 1, upper = I> ii[N];
  int<lower = 1, upper = J> jj[N];
  int<lower = 1, upper = K> kk[N];
  int<lower = 0, upper = 1> y[N];
}
parameters {
  vector[J] theta;
  vector[I] logalpha_z;
  vector[I] beta_z;
  vector[K] delta_z;
  real<lower = 0> sigma_alpha;
  real<lower = 0> sigma_beta;
  real<lower = 0> sigma_delta;
  real mu_beta;
}
transformed parameters {
  vector<lower = 0>[I] alpha = exp(logalpha_z * sigma_alpha);
  vector[I] beta = mu_beta + beta_z * sigma_beta;
  vector[K] delta = delta_z * sigma_delta;
}
model {
  theta ~ std_normal();
  logalpha_z ~ std_normal();
  beta_z ~ std_normal();
  delta_z ~ std_normal();
  sigma_alpha ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_delta ~ cauchy(0, 5);
  mu_beta ~ cauchy(0, 5);
  y ~ bernoulli_logit(alpha[ii] .* (theta[jj] + beta[ii] + delta[kk]));
}
