data {
  int<lower=0> N; // # of pixels * time steps
  int<lower=0> J; // # of pixels
  int<lower=0> P; // # of environment vars
  int<lower=0> Q; // # of soil vars
  array[N] int<lower=1,upper=N> pid; // pixel count
//  vector<lower=1,upper=N>[N] pid; // pixel count
  matrix[J,P] x; // NxP environmental matrix
  matrix[J,Q] s; //NxQ soil matrix
  vector<lower=-1>[N] age; // age at observation N
  vector<lower=-1,upper=1>[N] y_obs; // ndvi at observation N
  // a switch to evaluate the likelihood following:
  // https://khakieconomics.github.io/2017/-6/30/An-easy-way-to-simulate-fake-data-in-stan.html
  int<lower = 0, upper = 1> fit; // fit the model? Or just run with the priors
  int<lower = 0, upper = 1> predict; // predict NDVI for all pixels?
}
parameters {
}


generated quantities {

  // parameters
    //vector<lower=0>[J] alpha;
    vector[J] alpha;
    //vector<lower=0>[J] gamma;
    vector[J] gamma;
    vector[J] lambda;
    vector[P] gamma_beta;
    vector[P] lambda_beta;
    vector[Q] alpha_beta;
    //real<lower=0, upper=1> alpha_mu;

  // transformed parameters

      vector[N] mu;
      vector[J] gamma_mu;
      vector[J] lambda_mu;
      vector[J] alpha_mu;

      real<lower = 0> alpha_tau;
      real<lower = 0> gamma_tau;
      real<lower = 0> lambda_tau;
      real<lower = 0> tau;


  // model

    // hyperpriors
      alpha_tau = student_t_rng(4,0,1);
      gamma_tau = student_t_rng(4,0,1);
      lambda_tau = student_t_rng(4,0,1);
      tau = student_t_rng(4,0,1);


    // priors
      for (q in 1:Q) alpha_beta[q] = normal_rng(0.15, 3);
      for (p in 1:P) gamma_beta[p] = normal_rng(0, 3);
      for (p in 1:P) lambda_beta[p] = normal_rng(0, 3);


    // regressions
        gamma_mu = x*gamma_beta;
        lambda_mu = x*lambda_beta;
        alpha_mu = s*alpha_beta;

    // recovery curve
        for (j in 1:J) alpha[j] = normal_rng(alpha_mu[j],alpha_tau);
        for (j in 1:J) gamma[j] = normal_rng(gamma_mu[j],gamma_tau);
        for (j in 1:J) lambda[j] = normal_rng(lambda_mu[j],lambda_tau);


      for (i in 1:N){
        mu[i] = exp(alpha[pid[i]])+exp(gamma[pid[i]])-exp(gamma[pid[i]])*exp(-(age[i]/exp(lambda[pid[i]])));
    //    mu = exp(alpha[pid])+exp(gamma[pid])-exp(gamma[pid])*exp(-(age/exp(lambda[pid])));
      }



  //generated quantities

    array[N] real y_pred;

    y_pred = normal_rng(mu, tau);

}
