source("Code/CWM_functions.R")

# Pull in the species list and trait database

pca_df <- read.csv("Data/Derived/pca_variables.csv")


svspp_vec <- unique(pca_df$svspp)


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


# Bring in Stat regions for aggregation

library(sf)
library(tidyverse)

stat <- sf::read_sf("Data/Spatial/Statistical_Areas-selected/Statistical_Areas_2010_withNames.shp") %>%
  st_make_valid()

pj_crs <- st_crs(stat)

plot(stat["geometry"])

stat$Id[st_is_valid(stat)==F]

stat <- stat %>%
  filter(Id != 706) %>% # Get rid of the one geometry that is invalid.
  select(Id, geometry)


# Estimate CWM at the scale of the stat region

biomass_mat <- df %>%
  st_as_sf(coords = c("decdeg_beglon", "decdeg_beglat")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  st_transform(crs = pj_crs) %>%
  st_join(stat) %>%
  st_drop_geometry() %>%
  ungroup() %>%
  group_by(est_year, season, Id, svspp) %>%
  summarize(biomass_kg = sum(biomass_kg, na.rm = T)) %>% # Here I'm summing the biomass for each species in each block in each season and year.
  select(svspp, Id, est_year, season, biomass_kg) %>%
  pivot_wider(names_from = svspp, values_from = biomass_kg, values_fill = 0, names_sort = T)

bio.mat <- as.matrix(biomass_mat[,-c(1:3)])


pca.mat <- as.matrix(pca_df)[, c(1:3)]

cwm_mat <- CWM(T_mat = pca.mat, S_mat = bio.mat) # Run the community weighting function. 

cwm_df <- biomass_mat[,c(1:3)] %>%
  bind_cols(as.data.frame(cwm_mat)) %>% 
  as_tibble()

# Compare with FD::functcomp() estimate
row.names(pca.mat) <- pca_df$svspp
temp <- FD::functcomp(pca.mat, bio.mat)  

temp[1:6, 1] # Estimates are the same
cwm_df$PC1[1:6] # Estimates are the same


write.csv(cwm_df, "Data/Derived/CWM_pca_variables.csv", row.names = F, quote = F)
