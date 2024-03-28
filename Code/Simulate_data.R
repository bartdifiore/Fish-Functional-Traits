# Simulate some data


library(tidyverse)






sim_dat <- function(n_y = 50, n_r = 4, plot = plot){
  
  n = n_y*n_r
  
  # Predictors 
  year = 1:n_y
  year_vec = rep(year, n_r)
  
  region = 1:n_r
  region_vec = rep(region, each = n_y)
  
  shape <- c(1, 4, 3, 6)
  

  harvest <- matrix(nrow = n_y, ncol = n_r)
  means <- rnorm(n_r, 1000, sd = 100)
  for(r in 1:n_r){
      harvest[,r] = rnorm(n = n_y, mean = means[r], sd = 10)
  }
  
  
  temp <- matrix(nrow = n_y, ncol = n_r)
  means <- c(8, 16, 22, 4)
  for(r in 1:n_r){
    temp[,r] = rnorm(n = n_y, mean = means[r], sd = 0.5)
  }
  

  
  #parameters
  
  alpha_0 = 0
  beta_T = 1.5
  beta_H = 1
  beta_NAO = -4
  x0 = c(100, 100, 100, 100)
  phi = 0.5
  gamma = rnorm(n_r, 0, 10)
  w = 2
  v = 3
  
  #NAO <- sin(seq(2*pi, length.out = n_y))
  NAO <- rnorm(n_y, mean = alpha_0 + beta_NAO*1:n_y, sd = 1)
  NAO_vec <- rep(NAO, time = n_r)
  
  
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

  if(plot == T){
    
    sim_df = data.frame(y = c(y_data), x = c(x), year = year_vec, NAO = NAO_vec, temperature = c(temp), harvest = c(harvest), region = as.factor(region_vec))
    p1 <- ggplot(sim_df, aes(x = year , y = temperature))+
      geom_line(aes(color = as.factor(region)))
    
    p2 <- ggplot(sim_df, aes(x = year , y = harvest))+
      geom_line(aes(color = region))
    
    p3 <- ggplot(sim_df, aes(x = year , y = NAO))+
      geom_line()
    
    p4 <- ggplot(sim_df, aes(x = NAO, y = y))+
      geom_point(aes(color = as.factor(region)))
    
    p5 <- ggplot(sim_df, aes(x = harvest, y = y))+
      geom_point(aes(color = region))
    
    p6 <- ggplot(sim_df, aes(x = temperature, y = y))+
      geom_point(aes(color = region))
    
    p7 <- ggplot(sim_df, aes(x = year, y = x))+
      geom_line(aes(color = as.factor(region)))
    
    p8 <- ggplot(sim_df, aes(x = year, y = y))+
      geom_line(aes(color = as.factor(region)))
    
    out_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4, ncol = 2, align = "hv" )
    return(out_plot)
    
  }
  if(plot == F){
    output = list(n_y = n_y,
                  n_r = n_r,
                  y_dat = y_data,
                  NAO = NAO,
                  Temp = temp,
                  Harvest = harvest)
    return(output)
  }
    
}



sim_df <- sim_dat(plot = T)
library(tidyverse)





# Fit the stan model to the simulated data


mod <- rstan::stan_model("Multilevelregression_ar1_varyingintercepts_noncentered.stan")



stan_data <- sim_dat(plot = F)

stanfit <- rstan::sampling(mod,
                           data = stan_data, 
                           chains = 3,
                           thin = 1,
                           iter = 2500,
                           seed = 123123)




# Test it with Adams data

df <- read.csv("Data/Junk/unscaled_spectra_predictor_df.csv") %>%
  filter(year >= 1982) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  janitor::clean_names() %>%
  drop_na()


gsi <- df %>% distinct(year, gsi) %>% 
  select(gsi)
gsi <- as.vector(gsi$gsi)

temp <- df %>% select(year, spectra_area, sst) %>% 
  pivot_wider(names_from = spectra_area, values_from = sst, values_fill = mean(df$sst, na.rm = T)) %>%
  select(-year) %>%
  as.matrix()

harvest <- df %>% select(year, spectra_area, commercial_landings) %>% 
  mutate(commercial_landings = as.vector(scale(commercial_landings))) %>%
  pivot_wider(names_from = spectra_area, values_from = commercial_landings, values_fill = 0) %>%
  select(-year) %>%
  as.matrix()

y <- df %>% select(year, spectra_area, isd_exponent) %>% 
  pivot_wider(names_from = spectra_area, values_from = isd_exponent, values_fill = mean(df$isd_exponent, na.rm = T)) %>%
  select(-year) %>%
  as.matrix()

stan_data <- list(n_y = length(unique(df$year)),
                  n_r = length(unique(df$spectra_area)),
                  y_dat = y,
                  NAO = gsi,
                  Temp = temp,
                  Harvest = harvest)



mod <- rstan::stan_model("Multilevelregression_ar1_varyingintercepts_noNAO.stan")


x0 <- df %>% 
  filter(year == min(year)) %>% 
  select(year, spectra_area, isd_exponent)

x0 <- as.vector(x0$isd_exponent)

stan_data <- list(n_y = length(unique(df$year)),
                  n_r = length(unique(df$spectra_area)),
                  y_dat = y,
                  Temp = temp,
                  Harvest = harvest, 
                  x0 = x0)



stanfit <- rstan::sampling(mod,
                           data = stan_data, 
                           chains = 3,
                           thin = 5,
                           iter = 5000,
                           seed = 123123)


library(tidybayes)

stanfit %>%
  recover_types(df) %>%
  gather_draws(alpha_0, beta_H, beta_T, gamma[spectra_area], phi) %>% 
  median_qi()

stanfit %>%
  recover_types(df) %>%
  gather_draws(mu_pop[year]) %>% 
  median_qi() %>%
  ggplot(aes(x = year, y = .value))+
  geom_line()



stanfit %>%
  recover_types(df) %>%
  spread_draws(x)

names(stanfit)

library(rstanarm)

stan_glmer()



