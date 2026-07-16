data{
  int N;
  array[N] int species_index;
  vector[N] log_maximum_load_kn;
}

parameters{
  //mei parameters
  real<lower=0> sigma; //variation in MEI
  vector[3] ml; //mein mei per year
  vector[3] ml_pred; // marginailized posterior predictions of MEI
}

model{
    // for mei
  vector[N] mu;
  ml ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );

  for ( j in 1:N ) {
    mu[j] = ml[species_index[j]];
  }

log_maximum_load_kn ~ normal( mu , sigma );

for (i in 1:3) {
  ml_pred[i] ~ normal( ml[i] , sigma ) ;
}
