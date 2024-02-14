data {
  int<lower=0> N; // # of pixels * time steps
  int<lower=0> J; // # of pixels
  int<lower=0> P; // # of environment vars
  array[N] int<lower=1,upper=N> pid; // pixel count
  matrix[J,P] x; // JxP environmental matrix
  vector<lower=-1>[N] age; // age at observation N
  vector<lower=1,upper=12>[N] firemonth; // month of previous fire {1,12}
  vector<lower=-1,upper=1>[N] y_obs; // ndvi at observation N
  // a switch to evaluate the likelihood following:
  // https://khakieconomics.github.io/2017/-6/30/An-easy-way-to-simulate-fake-data-in-stan.html
  int<lower = 0, upper = 1> fit; // fit the model? Or just run with the priors
  int<lower = 0, upper = 1> predict; // predict NDVI for all pixels?
}

parameters {
  vector<lower=0, upper = 1>[J] alpha;
  vector<lower=0, upper = 1>[J] gamma;
  vector<lower=0>[J] lambda;
  vector<lower=0, upper = 1>[J] A;
  real<lower=0,upper=1> alpha_mu;
  vector[P] gamma_beta;
  vector[P] lambda_beta;
  vector[P] A_beta;
  real phi;
  real<lower=0> tau_sq;
  real<lower=0> gamma_tau_sq;
  real<lower=0> lambda_tau_sq;
  real<lower=0> alpha_tau_sq;
  real<lower=0> A_tau_sq;
  corr_matrix[4] Omega_m;        // prior correlation, presumably 4 because there are 4 parameters we're estimating?
  vector[4] Omega_v;      // prior scale
  matrix[4,4]Sigma;
  vector[4] mn[J]; //multivariate normal. 4 (# parms in the nmvnormal) x J (# pixels)

  vector<lower=0, upper = 1>[J] alpha_est;
  vector<lower=0, upper = 1>[J] gamma_est;
  vector<lower=0>[J] lambda_est;
  vector<lower=0, upper = 1>[J] A_est;

}

transformed parameters {
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

  if(fit==1){ // only run if fitting is desired
  for (i in 1:N){
   // mu[i] = alpha[pid[i]]+gamma[pid[i]]-gamma[pid[i]]*exp(-(age[i]/lambda[pid[i]]))+
   // sin((phi+((firemonth[i]-1)*3.141593/6))+6.283185*age[i])*A[pid[i]];
   // // mu = exp(alpha[pid])+exp(gamma[pid])-exp(gamma[pid])*exp(-(age/exp(lambda[pid])));
    mu[i] = mn[pid[i],1]+mn[pid[i],2]-mn[pid[i],2]*exp(-(age[i]/mn[pid[i],3]))+
    sin((phi+((firemonth[i]-1)*3.141593/6))+6.283185*age[i])*mn[pid[i],4];

  // mn[pid[i],1] = alpha[pid[i]]
  // mn[pid[i],2] = gamma[pid[i]]
  // mn[pid[i],3] = lambda[pid[i]]
  // mn[pid[i],4] = A[pid[i]]


  }
  }
}

model {

  // hyperpriors
  tau ~  student_t(10,0,0.5); //#inv_gamma(0.01, 0.01);
  gamma_tau ~ student_t(10,0,0.5); //#inv_gamma(0.01, 0.01);
  lambda_tau ~ student_t(10,0,0.5); //#inv_gamma(0.01, 0.01);
  alpha_tau ~ student_t(10,0,0.5); //#inv_gamma(0.01, 0.01);
  A_tau ~ student_t(10,0,0.5); //#inv_gamma(0.01, 0.01);

  // priors
  alpha_mu ~ normal(0.2,.2);
  gamma_beta ~ normal(0,3);
  lambda_beta ~ normal(0,3);
  A_mu ~ normal(0,3);
  // month effects
  phi  ~ uniform(-3.141593,3.141593);
  //multivariate normal correlation matrix prior
  Omega_m ~ lkj_corr(3); //was 2
  Omega_v ~ cauchy(0, 2.5);

// real mn_mus[J,4];
vector[4] mn_mus[J];

      #replace this for loop code that assigns by columns instead of rows?
      for (i in 1:J){

      mn_mus[i,1] = alpha[i];
      mn_mus[i,2] = gamma[i];
      mn_mus[i,3] = lambda[i];
      mn_mus[i,4] = A[i];


    }


  // // recovery curve - @Glenn - why the switch to normal from lognormal?
  // alpha ~ lognormal(alpha_mu, alpha_tau);
  // gamma ~ lognormal(gamma_mu,gamma_tau);
  // lambda ~ lognormal(lambda_mu,lambda_tau);
  // A ~ lognormal(A_mu, A_tau);

  //y ~ multi_normal(mu, Sigma): mu = vector of means, Sigma = covariance matrix
    //Sigma ~ quad_form_diag(matrix m, vector v) =  diag_matrix(v) * m * diag_matrix(v)
        // diag_matrix(v) =correlation matrix, m = correlation scale
    //mu = alpha_mu, gamma_mu, lambda_mu, A_mu for a given size
  //Sigma ~ quad_form_diag(Omega_m, Omega_v);



    mn  ~ multi_normal(mn_mus, quad_form_diag(Omega_m, Omega_v));


  for (i in 1:J){

    // alpha[i] ~ lognormal(alpha_mu, alpha_tau);
    // gamma[i] ~ lognormal(gamma_mu[i], gamma_tau);
    // lambda[i] ~ lognormal(lambda_mu[i], lambda_tau);
    // A[i] ~ lognormal(A_mu[i], A_tau);
    //
    // alpha_est[i] ~ lognormal(alpha_mu, alpha_tau);
    // gamma_est[i] ~ lognormal(gamma_mu[i], gamma_tau);
    // lambda_est[i] ~ lognormal(lambda_mu[i], lambda_tau);
    // A_est[i] ~ lognormal(A_mu[i], A_tau);


    alpha[i] ~ normal(alpha_mu, alpha_tau);
    gamma[i] ~ normal(gamma_mu[i], gamma_tau);
    lambda[i] ~ normal(lambda_mu[i], lambda_tau);
    A[i] ~ normal(A_mu[i], A_tau);

    alpha_est[i] ~ normal(alpha_mu, alpha_tau);
    gamma_est[i] ~ normal(gamma_mu[i], gamma_tau);
    lambda_est[i] ~ normal(lambda_mu[i], lambda_tau);
    A_est[i] ~ normal(A_mu[i], A_tau);


  //     //mn should be a vector of length 4: alpha_mu, gamma_mu, lambda_mu, A_mu for a given site

  //  y_obs ~ normal(mu, tau);

} // ENd i loop

}

generated quantities {

array[N] real y_pred;

    y_pred = normal_rng(mu, tau);

  }
