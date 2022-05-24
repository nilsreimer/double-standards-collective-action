data {
  int<lower = 1> N; // # of responses
  int<lower = 1> I; // # of actions
  int<lower = 1> J; // # of participants
  int<lower = 1> K; // # of conditions
  int<lower = 1, upper = I> ii[N];
  int<lower = 1, upper = J> jj[N];
  int<lower = 1, upper = K> kk[N];
  int<lower = 0, upper = 1> y[N];
  vector[N] x_sjb;
  vector[N] x_sup;
  vector[N] x_pol;
}
parameters {
  vector[J] theta;
  matrix[2, I] z_I;
  matrix[4, K] z_K;
  vector<lower = 0>[2] sigma_I;
  vector<lower = 0>[4] sigma_K;
  cholesky_factor_corr[2] L_I;
  cholesky_factor_corr[4] L_K;
  real mu_beta;
  real b_sjb;
  real b_sup;
  real b_pol;
}
transformed parameters {
  matrix[I, 2] r_I = transpose(diag_pre_multiply(sigma_I, L_I) * z_I);
  matrix[K, 4] r_K = transpose(diag_pre_multiply(sigma_K, L_K) * z_K); 
  vector<lower = 0>[I] alpha = exp(r_I[, 1]);
  vector[I] beta = mu_beta + r_I[, 2];
  vector[K] delta = r_K[, 1];
  vector[K] b_sjb_kk = b_sjb + r_K[, 2];
  vector[K] b_sup_kk = b_sup + r_K[, 3];
  vector[K] b_pol_kk = b_pol + r_K[, 4];
}
model {
  theta ~ std_normal();
  to_vector(z_I) ~ std_normal();
  to_vector(sigma_I) ~ cauchy(0, 5);
  L_I ~ lkj_corr_cholesky(2);
  to_vector(z_K) ~ std_normal();
  to_vector(sigma_K) ~ cauchy(0, 5);
  L_K ~ lkj_corr_cholesky(2);
  mu_beta ~ cauchy(0, 5);
  b_sjb ~ cauchy(0, 5);
  b_sup ~ cauchy(0, 5);
  b_pol ~ cauchy(0, 5);
  y ~ bernoulli_logit(alpha[ii] .* (theta[jj] + beta[ii] + delta[kk] + b_sjb_kk[kk] .* x_sjb + b_sup_kk[kk] .* x_sup + b_pol_kk[kk] .* x_pol));
}
generated quantities {
  corr_matrix[2] R_I = multiply_lower_tri_self_transpose(L_I);
  corr_matrix[4] R_K = multiply_lower_tri_self_transpose(L_K);
}
