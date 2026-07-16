data{
     int<lower=0> N;
     //array[N] int maximum_load_n;
     //vector[N weight_g;
     array[N] int species_index;
     vector[N] rupture_force_n;
}
parameters{
     real a;
     vector[3] as;
     real<lower=0> scale;
     real<lower=0> sigma;
}
model{
    vector[149] mu;
    sigma ~ normal( 0 , 1 );
    scale ~ exponential( 1 );
    as ~ normal( a , sigma );
    a ~ normal( 0 , 5 );
    for ( i in 1:149 ) {
        mu[i] = as[species_index[i]];
        mu[i] = exp(mu[i]);
    }
    rupture_force_n ~ gamma( mu/scale , 1/scale );
}
