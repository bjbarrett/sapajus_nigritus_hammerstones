data{
   int N;
   array[N] int species_index;
   vector[N] log_maximum_load_kn;
   
   int<lower=0> N_hamz;
   vector[N_hamz] weight_kg;
   array[N_hamz] int species_index_h;

}

parameters{
  real<lower=0> sigma; 
  vector[3] ml; 
  vector[3] ml_pred; 
  
  real a;
  real bh;
  vector[3] ah;
  real<lower=0> scale;
  real<lower=0> sigma_h; 

}

model{
  vector[N] mu;
  ml ~ normal( 1 , 0.5 );
  sigma ~ exponential( 1 );

  for ( j in 1:N ) {
    mu[j] = ml[species_index[j]];
  }

log_maximum_load_kn ~ normal( mu , sigma );

for (i in 1:3)  ml_pred[i] ~ normal( ml[i] , sigma ) ;


    vector[N_hamz] mu_h;
    sigma_h ~ normal(0,0.5);
    scale ~ exponential( 1 );
    ah ~ normal( 0 , sigma_h );
    a ~ normal( 0.5 , .75 );
    bh ~ normal(0 , 1);
    for ( i in 1:N_hamz ) {
        mu_h[i] = a + ah[species_index_h[i]] + bh*ml[species_index_h[i]];
        mu_h[i] = exp(mu_h[i]);
    }
    weight_kg ~ gamma( mu_h/scale , 1/scale );

}
