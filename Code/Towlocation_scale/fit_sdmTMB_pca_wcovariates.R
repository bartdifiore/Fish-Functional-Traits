library(tidyverse)
library(sdmTMB) # This is the workhorse to do the spatio-temporal modeling
library(sf)
source("Code/theme.R")
source("Code/CWM_functions.R")

#-----------------------------
## Read in the data
#-----------------------------

pca <- read.csv("Data/Derived/pca_variables.csv")

svspp_vec <- unique(pca$svspp)

covariates <- readRDS("Data/Derived/all_tows_all_covs.rds") %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(survey == "NMFS")

# Get trawl survey data

df_raw <- read.csv("Data/NMFS_trawl/NMFS_survdat_gmri_tidy.csv")

#NOTE!!!: Need to discuss with Adam and Kathy how Biomass_kg was generated. Adam mentioned that if abudance was >0 and biomass was zero/blank he filled with 0.0001. Why? Why not generate from the length-weight relationships? There are ~25000 entries with biomass_kg exactly equal to 0.0001, regardless of abundance. 

df <- df_raw %>% 
  select(svspp:biomass_kg, comname, strat_num, survey_area) %>% # Abundance and biomass_kg in this data set have already been corrected for differences in vessels. See <https://adamkemberling.github.io/nefsc_trawl/01_Survdat_Standard_Cleanup.html> for more details, and for details on which data has been filtered from the data set.
  distinct() %>% # the abundance and biomass_kg columns estimate the TOTAL abundance and biomass for a tow. They are repeated across each observation of individual fish for length distributions. Here I am concerned only with the aggregate abundance/biomass of each SPECIES. Therefore, distinct reduces the data frame to only the species level observations in each tow.
  group_by(across(-c(catchsex, abundance, biomass_kg))) %>% # Species that can be sexed (like DogFish, inverts) have two or more separate entries in a tow, one for each category. Here I'm grouping all other variables in order to summerize the total (sum()) abundance or biomass. 
  summarize(abundance = sum(abundance), 
            biomass_kg = sum(biomass_kg)) %>%
  filter(svspp %in% svspp_vec) # Filter out the data set for only the fish species that we found traits for. 


trawl_meta <- df %>% # Pull out the two level observations for merging back later
  ungroup() %>%
  select(cruise6:botsalin, strat_num, survey_area) %>% 
  distinct()

# Estimate CWM at the scale of the trawl

biomass_mat <- df %>% 
  ungroup() %>%
  select(svspp, cruise6, station, stratum, tow, est_year, season, biomass_kg) %>%
  pivot_wider(names_from = svspp, values_from = biomass_kg, values_fill = 0)

bio.mat <- as.matrix(biomass_mat[,-c(1:6)])

pca.mat <- as.matrix(pca)[, c(1:3)]

cwm_mat <- CWM(T_mat = pca.mat, S_mat = bio.mat) # Run the community weighting function. 

cwm_df <- biomass_mat[,c(1:6)] %>%
  bind_cols(as.data.frame(cwm_mat)) %>% 
  as_tibble()

# Compare with FD::functcomp() estimate
row.names(pca.mat) <- pca$svspp
temp <- FD::functcomp(pca.mat, bio.mat)  

temp[1:6, 1] # Estimates are the same
cwm_df$PC1[1:6] # Estimates are the same

write.csv(cwm_df, "Data/Derived/CWM_pca_trawl-scale.csv", row.names = F, quote = F)


# Merge w/ covariates

df <- cwm_df %>% 
  mutate(station_temp = formatC(station, width = 3, format = "d", flag = "0"),
         id = paste(cruise6, station_temp, stratum, sep = ""),
         season = toupper(season)) %>%
  select(id, est_year, season, PC1, PC2, PC3) %>%
  left_join(covariates %>% select(id, decdeg_beglat, decdeg_beglon, depth, bs_seasonal, bt_seasonal, ss_seasonal, sst_seasonal)) %>%
  drop_na(decdeg_beglon, decdeg_beglat) %>%
  add_utm_columns(ll_names = c("decdeg_beglon", "decdeg_beglat"), units = "km")

#----------------------------------------------
## Build a simple spatial random fields model
#----------------------------------------------

# Make the mesh
mesh <- make_mesh(df, c("X", "Y"), cutoff = 50)
plot(mesh)

m1 <- sdmTMB(
  data = df,
  formula = PC1 ~ poly(log(depth),2) + bt_seasonal,
  mesh = mesh,
  family = gaussian(link = "identity"),
  spatial = "on",
  time = "est_year",
  spatiotemporal = "IID"
)
sanity(m1)

m1 %>% 
  tidy(conf.int = T)


nd <- data.frame(
  depth = seq(min(df$depth),
              max(df$depth),
              length.out = 100
  ), 
  est_year = 2018, bt_seasonal = mean(df$bt_seasonal, na.rm = T))

p <- predict(m1, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se)
)) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  scale_x_continuous() +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Pace of Life (PCA1)")
ggsave("Figures/deptheffect.png")

nd <- data.frame(
  bt_seasonal = seq(min(df$bt_seasonal, na.rm = T),
              max(df$bt_seasonal, na.rm = T),
              length.out = 100
  ), 
  est_year = 2018, depth = mean(df$depth, na.rm = T))

p <- predict(m1, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(bt_seasonal, est,
              ymin = est - 1.96 * est_se,
              ymax = est + 1.96 * est_se)) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  scale_x_continuous() +
  coord_cartesian(expand = F) +
  labs(x = "Bottom temperature (C)", y = "Pace of Life (PCA1)")


nd <- data.frame(
  est_year = seq(min(df$est_year, na.rm = T),
                    max(df$est_year, na.rm = T)
  ), 
  bt_seasonal = mean(df$bt_seasonal, na.rm = T), depth = mean(df$depth, na.rm = T))

p <- predict(m1)



#---------------------------------------
## Plot predictions
#---------------------------------------

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











