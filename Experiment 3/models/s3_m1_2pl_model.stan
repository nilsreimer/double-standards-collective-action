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
  matrix[2, I] z_I;
  vector[K] z_K;
  vector<lower = 0>[2] sigma_I;
  real<lower = 0> sigma_K;
  cholesky_factor_corr[2] L_I;
  real mu_beta;
}
transformed parameters {
  matrix[I, 2] r_I = transpose(diag_pre_multiply(sigma_I, L_I) * z_I); 
  vector<lower = 0>[I] alpha = exp(r_I[, 1]);
  vector[I] beta = mu_beta + r_I[, 2];
  vector[K] delta = z_K * sigma_K;
}
model {
  theta ~ std_normal();
  to_vector(z_I) ~ std_normal();
  to_vector(sigma_I) ~ cauchy(0, 5);
  L_I ~ lkj_corr_cholesky(2);
  z_K ~ std_normal();
  sigma_K ~ cauchy(0, 5);
  mu_beta ~ cauchy(0, 5);
  y ~ bernoulli_logit(alpha[ii] .* (theta[jj] + beta[ii] + delta[kk]));
}
generated quantities {
  corr_matrix[2] R_I = multiply_lower_tri_self_transpose(L_I);
}
