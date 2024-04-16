data {
  int<lower = 1> N; // n of responses
  int<lower = 1> I; // n of actions
  int<lower = 1> J; // n of participants
  int<lower = 1> K; // n of conditions
  array[N] int<lower = 1, upper = I> ii;
  array[N] int<lower = 1, upper = J> jj;
  array[N] int<lower = 1, upper = K> kk;
  array[N] int<lower = 0, upper = 1> y;
  vector[N] x_mfq_care;
  vector[N] x_mfq_equa;
  vector[N] x_mfq_prop;
  vector[N] x_mfq_loya;
  vector[N] x_mfq_auth;
  vector[N] x_mfq_puri;
}
parameters {
  vector[J] theta;
  matrix[2, I] z_I;
  matrix[7, K] z_K;
  vector<lower = 0>[2] sigma_I;
  vector<lower = 0>[7] sigma_K;
  cholesky_factor_corr[2] L_I;
  cholesky_factor_corr[7] L_K;
  real mu_beta;
  real b_care;
  real b_equa;
  real b_prop;
  real b_loya;
  real b_auth;
  real b_puri;
}
transformed parameters {
  matrix[I, 2] r_I = transpose(diag_pre_multiply(sigma_I, L_I) * z_I);
  matrix[K, 7] r_K = transpose(diag_pre_multiply(sigma_K, L_K) * z_K); 
  vector<lower = 0>[I] alpha = exp(r_I[, 1]);
  vector[I] beta = mu_beta + r_I[, 2];
  vector[K] delta = r_K[, 1];
  vector[K] b_care_kk = b_care + r_K[, 2];
  vector[K] b_equa_kk = b_equa + r_K[, 3];
  vector[K] b_prop_kk = b_prop + r_K[, 4];
  vector[K] b_loya_kk = b_loya + r_K[, 5];
  vector[K] b_auth_kk = b_auth + r_K[, 6];
  vector[K] b_puri_kk = b_puri + r_K[, 7];
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
  b_care ~ cauchy(0, 5);
  b_equa ~ cauchy(0, 5);
  b_prop ~ cauchy(0, 5);
  b_loya ~ cauchy(0, 5);
  b_auth ~ cauchy(0, 5);
  b_puri ~ cauchy(0, 5);
  y ~ bernoulli_logit(alpha[ii] .* (theta[jj] + beta[ii] + delta[kk] + b_care_kk[kk] .* x_mfq_care + b_equa_kk[kk] .* x_mfq_equa + b_prop_kk[kk] .* x_mfq_prop + b_loya_kk[kk] .* x_mfq_loya + b_auth_kk[kk] .* x_mfq_auth + b_puri_kk[kk] .* x_mfq_puri));
}
generated quantities {
  corr_matrix[2] R_I = multiply_lower_tri_self_transpose(L_I);
  corr_matrix[7] R_K = multiply_lower_tri_self_transpose(L_K);
}
