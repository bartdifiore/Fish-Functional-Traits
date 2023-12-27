library(tidyverse)



# Pull in the species list and trait database

trait_db <- read.csv("Data/Derived/trait_database.csv")[, -1]
  

svspp_vec <- unique(trait_db$svspp)


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


biomass_mat <- df %>% 
  ungroup() %>%
  select(svspp, cruise6, station, stratum, tow, est_year, season, biomass_kg) %>%
  filter(!svspp %in% c(375, 725, 760, 768, 871)) %>% # species was missing l_inf, length_maturity... see comments below
  pivot_wider(names_from = svspp, values_from = biomass_kg, values_fill = 0)

bio.mat <- as.matrix(biomass_mat[,-c(1:6)])

trait_mat <- trait_db %>% 
  as_tibble() %>%
  select(svspp, habitat, feeding_mode, trophic_level, offspring_size, spawning_type, age_maturity, length_maturity,  fecundity, l_inf, k, max_obs_length) %>% # Subsetting out many traits to avoid dealing with NA's at this stage (12/27/23). 
  drop_na(l_inf, length_maturity) %>% # One species missing l_inf. filtering to avoid NA...
  group_by(svspp) %>%
  summarize(across(where(is.double), \(x) mean(x, na.rm = T)))

  
trait.mat <- as.matrix(trait_mat)[, -1]

cwm_mat <- CWM(T_mat = trait.mat, S_mat = bio.mat) # Run the community weighting function. 

cwm_df <- biomass_mat[,c(1:6)] %>%
  bind_cols(as.data.frame(cwm_mat)) %>% 
  as_tibble()

# Compare with FD::functcomp() estimate
row.names(trait.mat) <- trait_mat$svspp
temp <- FD::functcomp(trait.mat, bio.mat)  

temp[1:6, 1] # Estimates are the same
cwm_df$trophic_level[1:6] # Estimates are the same


cwm_df2 <- cwm_df %>% 
  left_join(trawl_meta)

cwm_df2 %>% 
  group_by(est_year, season) %>% 
  summarize(across(trophic_level:max_obs_length, mean)) %>%
  group_by(est_year, season) %>% 
  pivot_longer(cols = trophic_level:max_obs_length) %>%
  ggplot(aes(x = est_year, y = value))+
  geom_line(aes(color = season))+
  facet_wrap(~name, scales = "free")

















