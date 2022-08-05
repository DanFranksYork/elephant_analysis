data{
    int n_dyads;
    vector[n_dyads] together; // times elephants have been observed together
    vector[n_dyads] apart;    // times observed apart
    vector[n_dyads] age_diff_mean_centred; // age difference between elephants
    int node_1[n_dyads];      // nodeIDs for multimembership effects
    int node_2[n_dyads];
}

parameters {
  vector<lower=0,upper=1>[n_dyads] weight; 
  real alpha;
  real beta;
  real u[n_dyads];
  real<lower=0> sigma;
  real<lower=0> sigma_u; // multi-membership random effect (undirected connections)
}

model {
  // model for estimating the weight for the connection between the elephants
  weight ~ beta( 2 + together, 2 + apart ); // 2,2 weak prior for edge weights

  // model for dyadic regression of attributes on the connection weight
  for (i in 1:n_dyads) {
    logit(weight[i]) ~ normal(alpha + beta * age_diff_mean_centred[i] + u[node_1[i]] + u[node_2[i]], sigma);
  }

  u ~ normal(0, sigma_u);
  sigma_u ~ exponential(1);
  sigma ~ exponential(1);
  alpha ~ normal(-1.5, 2);
  beta ~ normal(0, 0.5);
}
