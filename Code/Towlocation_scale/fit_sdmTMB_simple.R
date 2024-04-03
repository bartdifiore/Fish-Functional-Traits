setwd("/Users/bart/Github/Fish-Functional-Traits")

library(tidyverse)
library(sdmTMB) # This is the workhorse to do the spatio-temporal modeling
library(sf)
source("Code/theme.R")
#-----------------------------
## Read in the data
#-----------------------------

df <- read.csv("Data/Derived/CWM_dataset.csv") %>% 
  as_tibble() %>%
  add_utm_columns(ll_names = c("decdeg_beglon", "decdeg_beglat"), units = "km")


#----------------------------------------------
## Build a simple spatial random fields model
#----------------------------------------------

# Make the mesh
mesh <- make_mesh(df, c("X", "Y"), cutoff = 30)

# Fit a spatiotemporal model

m_offspringsize <- sdmTMB(
  data = df,
  formula = offspring_size ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "ar1"
)

m_agematurity <- sdmTMB(
  data = df,
  formula = age_maturity ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

m_lengthmaturity <- sdmTMB(
  data = df,
  formula = length_maturity ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

m_fecundity <- sdmTMB(
  data = df,
  formula = fecundity ~ 1,
  mesh = mesh,
  family = gaussian(link = "log"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

m_linf <- sdmTMB(
  data = df,
  formula = l_inf ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

m_k <- sdmTMB(
  data = df,
  formula = k ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

m_obslength <- sdmTMB(
  data = df,
  formula = max_obs_length ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)

#---------------
## Set up maps
#---------------

points <- df %>% 
  select(X,Y) %>% 
  st_as_sf(coords = c("X","Y"))

boundary <-  df %>% 
  select(X,Y) %>% 
  st_as_sf(coords = c("X","Y")) %>% 
  st_union() %>%
  st_concave_hull(ratio = 0.01, allow_holes = F) %>% 
  st_buffer(dist = 10)


plot(boundary, col = "red")
plot(points, add = T, cex = 0.01)

pred_grid <- df %>% 
  select(X,Y) %>% 
  st_as_sf(coords = c("X","Y")) %>% 
  st_make_grid(cellsize = 10, what = "centers") %>% 
  st_intersection(boundary)

plot(boundary, col = "red")
plot(points, add = T, cex = 0.01)
plot(pred_grid, add = T)

pred_grid_df <- as.data.frame(st_coordinates(pred_grid))


grid_yrs <- replicate_df(pred_grid_df, "est_year", unique(df$est_year))



extent <- df %>%
  st_as_sf(coords = c("decdeg_beglon", "decdeg_beglat"), crs = "+proj=longlat +datum=WGS84") %>%
  st_bbox()

extent <- extent + c(-1, -2, 1, 0.5) # Add a little buffer for visualization purposes


coast_utm <- rnaturalearth::ne_coastline(scale = 10) %>%
  st_crop(extent) %>%
  st_transform(crs = "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


#------------------------------------------
## Function for predictions and plots
#-----------------------------------------

trait <- names(df)[7:14]

pred_sdmTMB <- function(trait, model){
  predictions = predict(model, newdata = grid_yrs)
  
  predictions_m = predictions %>% 
    mutate(X.m = X*1000, 
           Y.m = Y*1000, 
           decade = case_when(est_year < 1980 ~ "1970-1979", 
                              est_year >= 1980 & est_year < 1990 ~ "1980-1989", 
                              est_year >= 1990 & est_year < 2000 ~ "1990-1999", 
                              est_year >= 2000 & est_year < 2010 ~ "2000-2009", 
                              est_year >= 2010 ~ "2010-2019")) %>% 
    group_by(X.m, Y.m, decade) %>%
    summarize(mean_est = mean(est)) %>%
    ggplot()+
    geom_raster(aes(x = X.m, y = Y.m, fill = mean_est))+
    geom_sf(data = coast_utm) +
    scale_fill_viridis_c()+
    labs(x = "", y = "")+
    facet_wrap(~decade)+
    theme_bw()+
    ggtitle(paste("Model prediction of community weighted mean", trait, "\n(Averaged across decades)"))
  
          
  ggsave(filename = paste("Figures/Model_predictions-", trait, ".png", sep = ""), width = 10, height = 7)
  
}

pred_sdmTMB("offspring size", m_offspringsize)
pred_sdmTMB("age maturity", m_agematurity)
pred_sdmTMB("length maturity", m_lengthmaturity)
pred_sdmTMB("fecundity", m_fecundity)
pred_sdmTMB("l inf", m_linf)
pred_sdmTMB("VBN growth", m_k)
pred_sdmTMB("maximum observed length", m_obslength)












