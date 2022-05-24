functions {
  real induced_dirichlet_lpdf(vector c, vector alpha, real phi) {
    int K = num_elements(c) + 1;
    vector[K - 1] sigma = inv_logit(phi - c);
    vector[K] p;
    matrix[K, K] J = rep_matrix(0, K, K);
    
    // Induced ordinal probabilities
    p[1] = 1 - sigma[1];
    for (k in 2:(K - 1))
      p[k] = sigma[k - 1] - sigma[k];
    p[K] = sigma[K - 1];
    
    // Baseline column of Jacobian
    for (k in 1:K) J[k, 1] = 1;
    
    // Diagonal entries of Jacobian
    for (k in 2:K) {
      real rho = sigma[k - 1] * (1 - sigma[k - 1]);
      J[k, k] = - rho;
      J[k - 1, k] = rho;
    }
    
    return   dirichlet_lpdf(p | alpha)
           + log_determinant(J);
  }
}
data {
  int<lower = 1> I; // # of actions
  int<lower = 1> J; // # of participants
  int<lower = 1> N; // # of responses
  int<lower = 2> K; // # of response options
  int<lower = 1,upper = I> ii[N];
  int<lower = 1,upper = J> jj[N];
  int<lower = 0> y[N];
}
parameters {
  ordered[K-1] beta[I];
  vector[J] theta_z;
  vector[I] logalpha_z;
  real<lower = 0> sigma_theta;
  real<lower = 0> sigma_alpha;
}
transformed parameters {
  vector[J] theta = theta_z * sigma_theta;
  vector<lower = 0>[I] alpha = exp(logalpha_z * sigma_alpha);
}
model {
  theta_z ~ normal(0, 1);
  logalpha_z ~ normal(0, 1);
  sigma_theta ~ cauchy(0, 3);
  sigma_alpha ~ cauchy(0, 3);
  for (i in 1:I)
    beta[i] ~ induced_dirichlet(rep_vector(1, K), 0);
  for (n in 1:N)
    y[n] ~ ordered_logistic(theta[jj[n]] * alpha[ii[n]], beta[ii[n]]);
}
generated quantities {
  int<lower = 1, upper = K> y_pred[N];
  for (n in 1:N)
    y_pred[n] = ordered_logistic_rng(theta[jj[n]] * alpha[ii[n]], beta[ii[n]]);
}
