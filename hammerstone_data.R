library(bayesplot)

hamz <- read.csv("hammers_data.csv", sep=";")

#plot raw data
hamz$species_index <- as.integer(as.factor(hamz$species))
hamz$hammer_weight_kg <- hamz$hammer_weight_g/1000
hamz$log_hammer_weight_g <- log(hamz$hammer_weight_g)

#hammer weight
dens( hamz$hammer_weight_g[hamz$species_index==1] , col=fruit_pal[1], xlim=range(hamz$hammer_weight_g), xlab="hammer_weight_g")
dens( hamz$hammer_weight_g[hamz$species_index==2] , col=fruit_pal[2] , add=TRUE)
dens( hamz$hammer_weight_g[hamz$species_index==3] , col=fruit_pal[3] , add=TRUE)

#log hammer weight
dens( hamz$log_hammer_weight_g[hamz$species_index==1] , col=fruit_pal[1])
dens( hamz$log_hammer_weight_g[hamz$species_index==2] , col=fruit_pal[2] , add=TRUE)
dens( hamz$log_hammer_weight_g[hamz$species_index==3] , col=fruit_pal[3] , add=TRUE)

#log hammer weight

dens( hamz$hammer_thickness_mm[hamz$species_index==1] , col=fruit_pal[1] , xlim=range(hamz$hammer_thickness_mm), xlab="hammer_thickness_mm")
dens( hamz$hammer_thickness_mm[hamz$species_index==2] , col=fruit_pal[2] , add=TRUE)
dens( hamz$hammer_thickness_mm[hamz$species_index==3] , col=fruit_pal[3] , add=TRUE)
legend("topright" , legend=sort(unique(d$species_nut)) , fill=fruit_pal , bty='n' )

dens( hamz$hammer_width_mm[hamz$species_index==1] , col=fruit_pal[1] , xlim=range(hamz$hammer_width_mm))
dens( hamz$hammer_width_mm[hamz$species_index==2] , col=fruit_pal[2] , add=TRUE)
dens( hamz$hammer_width_mm[hamz$species_index==3] , col=fruit_pal[3] , add=TRUE)
legend("topright" , legend=sort(unique(d$species_nut)) , fill=fruit_pal , bty='n' )


dens( hamz$hammer_length_mm[hamz$species_index==1] , col=fruit_pal[1] , xlim=range(hamz$hammer_length_mm))
dens( hamz$hammer_length_mm[hamz$species_index==2] , col=fruit_pal[2] , add=TRUE)
dens( hamz$hammer_length_mm[hamz$species_index==3] , col=fruit_pal[3] , add=TRUE)
legend("topright" , legend=sort(unique(d$species_nut)) , fill=fruit_pal , bty='n' )


##redor list making
## mean per category
df2 <- list(
  rupture_force_n = d$rupture_force_n , 
  #weight_g = d$weight_g,
  species_index = d$species_index,
  N = nrow(d) ,
  log_maximum_load_kn = d$log_maximum_load_kn ,
  maximum_load_kn = d$maximum_load_kn/1000,
  N_hamz = nrow(hamz) , 
  weight_g = hamz$hammer_weight_g ,
  weight_kg = hamz$hammer_weight_kg ,
  log_weight_g = hamz$log_hammer_weight_g,
  species_index_h = hamz$species_index
)

#ruptire force ,ean
m_crush <-  ulam(
  alist(
    rupture_force_n ~ dgamma2(mu,scale), #unique shape and scale for used tools
    log(mu) <- a , #the regression to which we need to make it just jicaron
     a ~ normal(0,5), #prior for mean
    scale ~ exponential(1) # we need a big scale for weight
  ),
  
  data=df2, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)


### lets do some stan with max load
file <- file.path("max_load_me_weight.stan")
mod <- cmdstan_model(file)

fit4 <- mod$sample(
  data = df2,
  seed = 943,
  chains = 4,
  parallel_chains = 4,
  refresh = 100 ,# print update every 500 iters,
  adapt_delta = 0.95
)

draws4 <- fit4$draws(format = "df")
mcmc_hist(draws4)
mcmc_trace(draws4)
glorp <- c(dimnames(draws4)[[2]])
glorp <- glorp[-1 ]
for (i in 1:3) glorp <- glorp[-length(glorp)]

mcmc_areas(
  draws4, 
  prob = 0.8, # 80% intervals
  prob_outer = 0.95, # 99%
  point_est = "median",
  pars= glorp
)

plot(draws4$`ah[1]` , draws4$`ml[1]` , col= fruit_pal[1] , xlab="log_mass_hammerstone_kg" , ylab="log_maximum_load_kn" , xlim=c(-3,3) , ylim=c(-2,2))
points(draws4$`ah[2]` , draws4$`ml[2]` , col= fruit_pal[2] )
points(draws4$`ah[3]` , draws4$`ml[3]` , col= fruit_pal[3] )
legend("topright" , legend=sort(unique(d$species_nut)) , fill=fruit_pal , bty='n' )

