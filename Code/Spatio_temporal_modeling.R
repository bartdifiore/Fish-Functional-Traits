
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

#------------------------------------
## Look for temporal autocorrelation
#------------------------------------

for_acf <- df %>% 
  group_by(est_year) %>% 
  summarize(across(trophic_level:max_obs_length, mean))

traits <- names(df)[7:14]


png("Figures/temporal_autocorrelation.png",width = 1200, height = 1000)
d <- par(mfrow = c(2,4))
for(i in 1:length(traits)){
  acf(for_acf[names(for_acf) == traits[i]])
}
par(d)
dev.off()

#----------------------------------------------
## Build a simple spatial random fields model
#----------------------------------------------

# Make the mesh
mesh <- make_mesh(df, c("X", "Y"), cutoff = 30)

mesh$mesh$n
plot(mesh)

# Fit a spatiotemporal model

m1 <- sdmTMB(
  data = df,
  formula = trophic_level ~ 1,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "ar1"
)

m1
tidy(m1, "fixed", conf.int = TRUE)
tidy(m1, "ran_pars", conf.int = TRUE)
AIC(m1)    
resids <- residuals(m1) # randomized quantile residuals
qqnorm(resids)
qqline(resids) # Doesn't look very good yet

predictions <- predict(m1)
head(predictions)
summary(predictions$est)

predictions$resids <- resids# randomized quantile residuals

ggplot(predictions, aes(X, Y, col = resids)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~est_year)
hist(predictions$resids)
qqnorm(predictions$resids);abline(a = 0, b = 1)

# Build a prediction grid

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

# Predict the model onto the grid

grid_yrs <- replicate_df(pred_grid_df, "est_year", unique(df$est_year)) # Replicate the grid by year

# Make the predictions
predictions <- predict(m1, newdata = grid_yrs)
head(predictions) # data from of predictions on the grid for each year


predictions_m <- predictions %>% 
  mutate(X.m = X*1000, 
         Y.m = Y*1000) # Correct for the fact that we projected in KM!!! (only took me two days to find this mistake)
  # st_as_sf(coords = c("X.m", "Y.m"), crs = "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
  # st_transform(crs = "+proj=longlat +datum=WGS84")

# pred_grid_m <- pred_grid_df %>%
#   mutate(X.m = X*1000, 
#          Y.m = Y*1000) %>%
#   st_as_sf(coords = c("X.m", "Y.m"), crs = "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
  # st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  # select(geometry)


#predictions_in_grid <- st_join(predictions_sf, pred_grid_latlong, join = st_within)

# Bring in coastline shapefile

extent <- df %>%
  st_as_sf(coords = c("decdeg_beglon", "decdeg_beglat"), crs = "+proj=longlat +datum=WGS84") %>%
  st_bbox()
  
extent <- extent + c(-1, -2, 1, 0.5) # Add a little buffer for visualization purposes


coast_utm <- rnaturalearth::ne_coastline(scale = 10) %>%
  st_crop(extent) %>%
  st_transform(crs = "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
plot(coast_utm[1])


predictions_m %>%
  filter(est_year == 1990) %>%
  ggplot()+
  geom_raster(aes(x = X.m, y = Y.m, fill = est))+
  geom_sf(data = coast_utm) +
  scale_fill_viridis_c()+
  labs(x = "", y = "")+
  theme_bw()


p1 <- predictions_m %>%
  as_tibble() %>%
  mutate(decade = case_when(est_year < 1980 ~ "1970-1979", 
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
  ggtitle("Model prediction of community weighted mean trophic level\n(Averaged across decades)")

ggsave("Figures/Model_predictions-trophiclevel.png", p1)

p2 <- df %>% 
  group_by(est_year, season, survey_area) %>% 
  summarize(trophic_level = mean(trophic_level)) %>%
  ggplot(aes(x = est_year, y = trophic_level))+
  geom_line(aes(color = season))+
  facet_wrap(~survey_area)+
  theme_bd()+
  ggtitle("Community weighted mean trophic level\n(Averaged across space)")
ggsave("Figures/Raw-trophiclevel.png", p2)





m_lengthmaturity2 <- sdmTMB(
  data = df,
  formula = length_maturity ~ 0 + as.factor(est_year) ,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)


pred_grid_df <- as.data.frame(st_coordinates(pred_grid))

grid_yrs <- replicate_df(pred_grid_df, "est_year", unique(df$est_year))


predictions2 = predict(m_lengthmaturity2, newdata = grid_yrs, return_tmb_object = T)

test <- get_index(predictions2, area = 100, bias_correct = T)
test
