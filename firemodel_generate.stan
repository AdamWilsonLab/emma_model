data {
  int<lower=0> N; // # of pixels * time steps
  int<lower=0> J; // # of pixels
  int<lower=0> P; // # of environment vars
  int<lower=1,upper=J> pid[N]; // pixel count
  matrix[J,P] x; // NxP environmental matrix
  vector[N] age; // age at observation N
  vector[N] ndvi; // ndvi at observation N
  // a switch to evaluate the likelihood following:
  // https://khakieconomics.github.io/2017/-6/30/An-easy-way-to-simulate-fake-data-in-stan.html
  int<lower = 0, upper = 1> run_estimation;
}
parameters {
  vector[J] alpha;
  vector[J] gamma;
  vector[J] lambda;
  real alpha_mu;
  vector[P] gamma_beta;
  vector[P] lambda_beta;
  real<lower=0> tau_sq;
  real<lower=0> gamma_tau_sq;
  real<lower=0> lambda_tau_sq;
  real<lower=0> alpha_tau_sq;
}

transformed parameters {
  vector[N] mu;
  vector[J] gamma_mu;
  vector[J] lambda_mu;
  real tau = sqrt(tau_sq);
  real gamma_tau = sqrt(gamma_tau_sq);
  real lambda_tau = sqrt(lambda_tau_sq);
  real alpha_tau = sqrt(alpha_tau_sq);

// regressions
    gamma_mu = x*gamma_beta;
    lambda_mu = x*lambda_beta;

  for (i in 1:N){
//    bid[i]=pid[i]; //# links the dynamic data to the static env table
//    mu[i] = exp(alpha[bid])+exp(gamma[bid])-exp(gamma[bid])*exp(-(age[i]/exp(lambda[bid])));
    mu[i] = exp(alpha[pid[i]])+exp(gamma[pid[i]])-exp(gamma[pid[i]])*exp(-(age[i]/exp(lambda[pid[i]])));
  }
}

model {

  // hyperpriors
  tau ~  student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  gamma_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  lambda_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  alpha_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);

  // priors
  alpha_mu ~ normal(0.15,3);
  gamma_beta ~ normal(0,3);
  lambda_beta ~ normal(0,3);

  // recovery curve
  alpha ~ normal(alpha_mu, alpha_tau);
  gamma ~ normal(gamma_mu,gamma_tau);
  lambda ~ normal(lambda_mu,lambda_tau);

  // likelihood
  if(run_estimation==1){ // only run if desired
    ndvi ~ normal(mu, tau);
  }
}

generated quantities {
  vector[N] ndvi_pred;

  for (n in 1:N){
    ndvi_pred[n] = normal_rng(mu[n], tau);
  }
}
