data{
     int<lower=0> N;
    //array[N] int maximum_load_n;
     //vector[N weight_g;
     //array[N] int code;
     vector[N] rupture_force_n;
}
parameters{
     real a;
     real<lower=0> scale;
}
model{
    real mu;
    scale ~ exponential( 1 );
    a ~ normal( 0 , 5 );
    mu = a;
    mu = exp(mu);
    rupture_force_n ~ gamma( mu/scale , 1/scale );
}