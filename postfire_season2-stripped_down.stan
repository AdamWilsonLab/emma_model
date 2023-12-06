//this version will be stripped down

data {
  int<lower=0> N; // # of pixels * time steps
  int<lower=0> J; // # of pixels
  int<lower=0> P; // # of environment vars
  array[N] int<lower=1,upper=J> pid; // pixel identifier
  matrix[J,P] x; // NxP environmental matrix
  vector<lower=-1>[N] age; // age at observation N
  vector<lower=1,upper=12>[N] firemonth; // month of previous fire {1,12}
  vector<lower=-1,upper=1>[N] y_obs; // ndvi at observation N
  // a switch to evaluate the likelihood following:
  // https://khakieconomics.github.io/2017/-6/30/An-easy-way-to-simulate-fake-data-in-stan.html
  int<lower = 0, upper = 1> fit; // fit the model? Or just run with the priors
  int<lower = 0, upper = 1> predict; // predict NDVI for all pixels?
}
parameters {
  vector<lower=-1, upper = 1>[J] alpha; //note this was previously set at lower =0, which caused issues with prediction
  vector<lower=0, upper = 1>[J] gamma;
  vector<lower=0>[J] lambda;
  //vector<lower=0, upper = 1>[J] A;
  real<lower=-1,upper=1> alpha_mu; //note this was previously set at lower =0, which caused issues with prediction
  vector[P] gamma_beta;
  vector[P] lambda_beta;
  //vector[P] A_beta;
  // real phi;
  real<lower=0> tau_sq; // these force the solution to  be >=0
  real<lower=0> gamma_tau_sq; // ideally these taus could be specified as greater than zero, they throw an error at zero
  real<lower=0> lambda_tau_sq;
  real<lower=0> alpha_tau_sq;
  //real<lower=0> A_tau_sq;
}

transformed parameters {
  vector[N] mu;
  vector[J] gamma_mu;
  vector[J] lambda_mu;
  //vector[J] A_mu;
  real tau = sqrt(tau_sq);
  real gamma_tau = sqrt(gamma_tau_sq);
  real lambda_tau = sqrt(lambda_tau_sq);
  real alpha_tau = sqrt(alpha_tau_sq);
  //real A_tau = sqrt(A_tau_sq);


// regressions
    gamma_mu = x*gamma_beta;
    lambda_mu = x*lambda_beta;
    //A_mu = x*A_beta;

  if(fit==1){ // only run if fitting is desired
  for (i in 1:N){
    // mu[i] = alpha[pid[i]]+gamma[pid[i]]-gamma[pid[i]]*exp(-(age[i]/lambda[pid[i]]))+
    // sin((phi+((firemonth[i]-1)*3.141593/6))+6.283185*age[i])*A[pid[i]];
   mu[i] = alpha[pid[i]]+gamma[pid[i]]-gamma[pid[i]]*exp(-(age[i]/lambda[pid[i]]));

    // mu[i] = alpha[pid[i]];

  }
  }
} // tf parms


model {

  // hyperpriors
  // tau ~  student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  // gamma_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  // lambda_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  // alpha_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);
  // A_tau ~ student_t(4,0,1); //#inv_gamma(0.01, 0.01);


  // using a lognormal prior, as the variance needs to be positive.
  // constraining with the transformed parameters isn't helpful for predictions
  tau ~  lognormal(0, 1);
  gamma_tau ~ lognormal(0, 1);
  lambda_tau ~ lognormal(0, 1);
  alpha_tau ~ lognormal(0, 1);
  //A_tau ~ lognormal(0, 1);

  // priors
  alpha_mu ~ normal(0.2,.2);
  gamma_beta ~ normal(0,3);
  lambda_beta ~ normal(0,3);
  //A_beta ~ normal(0,3);
  //A_mu ~ normal(0,3);
  // month effects
  // phi  ~ uniform(-3.141593,3.141593);

  // recovery curve - @Glenn - why the switch to normal from lognormal?
  // alpha ~ lognormal(alpha_mu, alpha_tau);
  // gamma ~ lognormal(gamma_mu,gamma_tau);
  // lambda ~ lognormal(lambda_mu,lambda_tau);
  // A ~ lognormal(A_mu, A_tau);

// will need to be normal for mvnormal
  alpha ~ normal(alpha_mu, alpha_tau);
  gamma ~ normal(gamma_mu, gamma_tau);
  lambda ~ normal(lambda_mu, lambda_tau);
  //A ~ normal(A_mu, A_tau);


  // likelihood
//  if(fit==1){ // only run if fitting is desired
    y_obs ~ normal(mu, tau);
//  }
}


generated quantities {

  array[N] real mu_pred;
  array[N] real y_pred_from_parms;
  array[N] real y_pred;
  vector[J] alpha_pred;
  vector[J] gamma_pred;
  vector[J] lambda_pred;
  vector[J] lambda_debug;
  vector[J] lambda_debug2;


  y_pred = normal_rng(mu,tau);


  for(k in 1:J){

  alpha_pred[k] = normal_rng(alpha_mu, alpha_tau);
  gamma_pred[k] = normal_rng(gamma_mu[k], gamma_tau);
  lambda_pred[k] = normal_rng(lambda_mu[k], lambda_tau);
  lambda_debug[k] = k;
  lambda_debug2[k] = 1;


  }


  for (i in 1:N){

   mu_pred[i] = alpha_pred[pid[i]]+
                gamma_pred[pid[i]]-
                gamma_pred[pid[i]]*exp(-(age[i]/lambda_pred[pid[i]]));

   // mu_pred[i] = alpha_pred[pid[i]];


    y_pred_from_parms[i] = normal_rng(mu_pred[i], tau);

    } // i loop

  } //generated qs

