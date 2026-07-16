library(rethinking)
library(brms)
library(janitor)
## data cleaning
d <- read.csv("nuts_rupture.csv")
d <- clean_names(d)
str(d)
rethinking::dens(d$weight_g [])
sss <- which(d$weight_g>100)
d[sss,]$weight_g <- d[sss,]$weight_g*.001 #coorect weights
grepl( "hole", d, fixed = TRUE)
d$hole <- grepl( "hole", d$species_nut) # is there a hole in fruit
d$species

d$species_nut <- gsub("_hole", " ", d$species_nut)
d$species_nut <- gsub(" ", "", d$species_nut)

d <- d[d$maximum_load_n > 0,]
d <- d[complete.cases(d),]
d$species_index <- as.integer(as.factor(d$species_nut))
d$log_maximum_load_kn <- log(d$maximum_load_n/1000)
d$maximum_load_kn <- d$maximum_load_n/1000

m1 <- lm(weight_g ~ species_nut , data=d )
summary(m1)
# https://rpubs.com/jwesner/gamma_glm

## brms glm weight of nut by secies
# brm_glm_reg <- brm(weight_g~species_nut, data=d, family=Gamma(link="log"),
#                    prior=c(prior(normal(0,2),class="Intercept"),
#                            prior(normal(0,2),class="b"),
#                            prior(gamma(0.01,0.01),class="shape")),
#                    chains=4,iter=1000, cores=4)
# 
# print(brm_glm_reg, prior=T)
# plot(marginal_effects(brm_glm_reg),points=T)
# plot(brm_glm_reg)
# 
# brm_glm_reg_2 <- brm(weight_g~species_nut , data=d, family=Gamma(link="log"),
#                    prior=c(prior(normal(0,2),class="Intercept"),
#                            prior(normal(0,2),class="b"),
#                            prior(gamma(0.01,0.01),class="shape")),
#                    chains=4,iter=1000, cores=4)
# 
# brm_glm_reg_3 <- brm(rupture_force_n~species_nut, data=d, family=Gamma(link="log"),
#                    prior=c(prior(normal(0,2),class="Intercept"),
#                            prior(normal(0,2),class="b"),
#                            prior(gamma(0.01,0.01),class="shape")),
#                    chains=4,iter=1000, cores=4)
# print(brm_glm_reg_3, prior=T)
# 
# plot(marginal_effects(brm_glm_reg_3),points=T)
# plot(brm_glm_reg_3)
# 
# brm_glm_reg_4 <- brm(rupture_force_n~species_nut + weight_g, data=d, family=Gamma(link="log"),
#                      prior=c(prior(normal(0,2),class="Intercept"),
#                              prior(normal(0,2),class="b"),
#                              prior(gamma(0.01,0.01),class="shape")),
#                      chains=4,iter=1000, cores=4)
# 
# plot(marginal_effects(brm_glm_reg_4),points=T)
# plot(brm_glm_reg_4)

#rupture_force_n~species_nut*hole + weight_g*hole



#####start back up doing stan models
df <- data.frame(
  rupture_force_n=d$rupture_force_n , 
  weight_g = d$weight_g,
  species_nut_index <- as.integer(as.factor(d$species_nut)),
  log_maximum_load_kn <- d$log_maximum_load_kn,
  maximum_load_kn <- d$maximum_load_n/1000
  
)

#ulam intercept of force on gamma scale
m_crush <-  ulam(
  alist(
    maximum_load_kn ~ dgamma2(mu,scale), #unique shape and scale for used tools
    log(mu) <- a , #the regression to which we need to make it just jicaron
     a ~ normal(1,0.5), #prior for mean
    scale ~ exponential(1) # we need a big scale for weight
  ),
  
  data=d, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

#ulam 
m_crush2 <-  ulam(
  alist(
    rupture_force_n ~ dgamma2(mu,scale), #unique shape and scale for used tools
    log(mu) <- as[species_index], #the regression to which we need to make it just jicaron
    a ~ normal(0,5), #prior for mean
    as[species_index] ~ normal(a,sigma),
    scale ~ exponential(1), # we need a big scale for weight
    sigma ~ half_normal(0,1) # we need a big scale for weight
    
  ),
  
  data=d, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

rethinking::precis(m_crush2 , depth=2)
rethinking::stancode(m_crush2)
#How is hammer  related to crush force?

#####start back up doing stan models
df2 <- list(
  rupture_force_n = d$rupture_force_n , 
  weight_g = d$weight_g,
  species_index = d$species_index,
  N = nrow(d) ,
  log__load_kn = d$log_maximum_load_kn ,
  maximum_load_kn = d$maximum_load_kn/1000 
)

## mean crush force
file <- file.path("m1_crush.stan")
mod <- cmdstan_model(file)

fit1 <- mod$sample(
  data = df2,
  seed = 943,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
precis(fit1)
fit1$summary()

## mean per category
file <- file.path("m2_crush.stan")
mod <- cmdstan_model(file)

fit2 <- mod$sample(
  data = df2,
  seed = 943,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
precis(fit2)
fit2$summary()


## mean per category
file <- file.path("max_load_me.stan")
mod <- cmdstan_model(file)

fit3 <- mod$sample(
  data = df2,
  seed = 943,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit3$summary()


draws3 <- fit3$draws(format = "df")
mcmc_hist(draws3)

fruit_pal=c("#46edc8" , "#ff5978" , "#fbb35a")

d$hole_binary <- ifelse(d$hole , 1 , 0)
dens(draws3$`ml[1]` , col=fruit_pal[1]  , xlim=c(-5,5) , xlab="log maximum load (kN)" , ylim=c(-.2 , 3.2))
dens(draws3$`ml_pred[1]` , col=fruit_pal[1]  , lty=2 , add=TRUE)
klump <- d$log_maximum_load_kn[d$species_index==1]
klump2 <- d$hole_binary[d$species_index==1] + 1 #indexing for graph
points(klump , rep(0 ,length(klump) ) , col=fruit_pal[1] , pch=pch_bin[klump2])

dens(draws3$`ml[2]` , col=fruit_pal[2]  , add=TRUE)
dens(draws3$`ml_pred[2]` , col=fruit_pal[2] , lty=2  , add=TRUE)
klump <- d$log_maximum_load_kn[d$species_index==2]
#points(klump , rep(-.1 ,length(klump) ) , col=fruit_pal[2])
klump2 <- d$hole_binary[d$species_index==2] + 1 #indexing for graph
points(klump , rep(-.1 ,length(klump) ) , col=fruit_pal[2] , pch=pch_bin[klump2])

dens(draws3$`ml[3]` , col=fruit_pal[3]  , add=TRUE)
dens(draws3$`ml_pred[3]` , col=fruit_pal[3]  , lty=2 , add=TRUE)
klump <- d$log_maximum_load_kn[d$species_index==3]
klump2 <- d$hole_binary[d$species_index==3] + 1 #indexing for graph
#points(klump , rep(-.2 ,length(klump) ) , col=fruit_pal[3])
points(klump , rep(-.2 ,length(klump) ) , col=fruit_pal[3] , pch=pch_bin[klump2])

legend("topright" , legend=sort(unique(d$species_nut)) , fill=fruit_pal , bty='n' )
