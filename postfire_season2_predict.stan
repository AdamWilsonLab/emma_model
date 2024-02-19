## Adam's attempt to use model-based prediction following https://medium.com/@alex.pavlakis/making-predictions-from-stan-models-in-r-3e349dfac1ed
## Predict with stan method with two model scripts - first to fit with big data, second to predict new data using fitted params.
## not working - just an early draft


data {
  int<lower=0> N; // # of pixels * time steps
  int<lower=0> J; // # of pixels
  int<lower=0> P; // # of environment vars
  array[N] int<lower=1,upper=N> pid; // pixel count
  matrix[J,P] x; // NxP environmental matrix
  vector<lower=-1>[N] age; // age at observation N
  vector<lower=1,upper=12>[N] firemonth; // month of previous fire {1,12}
  vector<lower=-1,upper=1>[N] y_obs; // ndvi at observation N
}
parameters {}
model {}

generated quantities {

array[N] real y_pred;

  vector[N] mu;
  vector[J] gamma_mu;
  vector[J] lambda_mu;
  vector[J] A_mu;
  real tau = sqrt(tau_sq);
  real gamma_tau = sqrt(gamma_tau_sq);
  real lambda_tau = sqrt(lambda_tau_sq);
  real alpha_tau = sqrt(alpha_tau_sq);
  real A_tau = sqrt(A_tau_sq);

// regressions
    gamma_mu = x*gamma_beta;
    lambda_mu = x*lambda_beta;
    A_mu = x*A_beta;

  for (i in 1:N){
    mu[i] = alpha[pid[i]]+gamma[pid[i]]-gamma[pid[i]]*exp(-(age[i]/lambda[pid[i]]))+
    sin((phi+((firemonth[i]-1)*3.141593/6))+6.283185*age[i])*A[pid[i]];
    y_pred = normal_rng(mu, tau);
}
}

