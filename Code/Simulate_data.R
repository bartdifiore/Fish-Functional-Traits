# Simulate some data


sim_dat <- function(n_y = 50, n_r = 4){
  
  n = n_y*n_r
  
  # Predictors 
  year = 1:n_y
  year_vec = rep(year, n_r)
  
  region = 1:n_r
  region_vec = rep(region, each = n_y)
  
  shape <- c(1, 4, 3, 6)
  

  harvest <- matrix(nrow = n_y, ncol = n_r)
  means <- c(10^6, 10^5, 10^4, 10^3)
  for(r in 1:n_r){
      harvest[,r] = rnorm(n = n_y, mean = means[r], sd = 10)
  }
  
  
  temp <- matrix(nrow = n_y, ncol = n_r)
  means <- c(8, 16, 22, 4)
  for(r in 1:n_r){
    temp[,r] = rnorm(n = n_y, mean = means[r], sd = 0.5)
  }
  
  NAO <- sin(seq(4*pi, length.out = n_y))
  NAO_vec <- rep(NAO, time = n_r)
  
  
  #parameters
  
  alpha_0 = 0
  beta_T = -9
  beta_H = 10
  beta_NAO = 0.5
  x0 = c(100, 100, 100, 100)
  phi = 0.5
  gamma = rnorm(n_r, 0, 10)
  w = 0
  v = 0
  
  mu_pop = alpha_0 + beta_NAO * NAO
  
  mu_r <- matrix(nrow = n_y, ncol = n_r)
  
  for(y in 1:n_y){
    for(r in 1:n_r){
        mu_r[y,r] = mu_pop[y] + gamma[r] + beta_H * harvest[y,r] + beta_T * temp[y,r] 
    }
  }
  
  x_hat <- matrix(nrow = n_y, ncol = n_r)
  
  x_hat[1, ] = x0
    
  for(y in 2:n_y){
    for(r in 1:n_r){
      x_hat[y, r] = phi*x_hat[y-1, r] + mu_r[y,r] 
    }
  }

  x <- matrix(nrow = n_y, ncol = n_r)
  y_data <- matrix(nrow = n_y, ncol = n_r)
  
  for(y in 1:n_y){
    for(r in 1:n_r){
      x[y,r] = rnorm(1, x_hat[y,r], v)
      y_data[y,r] = rnorm(1, x[y,r], w)
    }
  }

  df = data.frame(y = c(y_data), x = c(x), year = year_vec, NAO = NAO_vec, temperature = c(temp), harvest = c(harvest), region = as.factor(region_vec))
  return(df)
  
  # output = list(n_y = n_y, 
  #               n_r = n_r, 
  #               y_dat = y_data, 
  #               NAO = NAO, 
  #               Temp = temp, 
  #               Harvest = harvest)
  # return(output)
  
}


sim_df <- sim_dat()
library(tidyverse)
ggplot(sim_df, aes(x = year , y = temperature))+
  geom_line(aes(color = as.factor(region)))

ggplot(sim_df, aes(x = year , y = harvest))+
  geom_line(aes(color = region))

ggplot(sim_df, aes(x = NAO, y = y))+
  geom_point(aes(color = as.factor(region)))

ggplot(sim_df, aes(x = harvest, y = y))+
  geom_point(aes(color = region))

ggplot(sim_df, aes(x = year, y = x))+
  geom_line(aes(color = as.factor(region)))




# Fit the stan model to the simulated data


mod <- rstan::stan_model("Multilevelregression_ar1_varyingintercepts.stan")


stan_data <- sim_df

stanfit <- rstan::sampling(mod,
                           data = stan_data, 
                           chains = 3,
                           thin = 1,
                           iter = 1000,
                           seed = 123123)


















