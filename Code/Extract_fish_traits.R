
# Traits of interest, code, and description
# 
# Categorical traits
  # habitat_type = from SOE report 
  # rep_strategy = reproductive strategy 
  # com_value = Is it the subject of a TARGETED fishery
  # schooling = does it school?

# Continuous traits
  # max_obs_length = maximum observed/reported length
  # l_inf = length at infinite age parameter estimates from the Von Bertalanffy equation
  # k = individual growth coefficient k estimated from the Von Bertalanffy equation
  # fecundity = absolute number of oocytes
  # trophic_level = trophic level (as defined by FishBase) 
  # rm = intrinsic rate of population increase
  # age_maturity = age at maturity
  # length_maturity = length at maturity
  # temperature = ??? occurs in the stocks table in fishbase. FishLife provides a single estimate without documentation

# Tracking variables
  # For each trait variable there will be a "_level" and a "_source" classification. For example, max_obs_length, will also have max_obs_length_source and max_obs_length_level. Source refers to if the metric came from FB, Beukoff, or FishLife. Level refers to the taxonomic level that the estimate came from. 

library(tidyverse)
library(janitor)


# Get species list

species_df <- read.csv("Data/Derived/species_dataframe.csv") %>%
  rename(species = validated_species)
species_list <- unique(species_df$species)


beuk <- read.csv("Data/Other_trait_databases/Beukoff_2019.csv") %>% 
  janitor::clean_names() %>%
  filter(fao %in% c(21, 31), lme %in% c(6,7,8,9)) %>%
  mutate(species = paste(genus, species, sep = " ")) %>%
  filter(species %in% unique(species_df$species)) %>%
  # select(family, genus, species, lme, fao, tl, level_tl, spawning_type, reference_spawning_type, age_maturity, level_age_maturity, fecundity, level_fecundity, length_infinity, growth_coefficient, level_growth, length_max, level_length_max) %>%
  rename(trophic_level = tl, 
         l_inf = length_infinity, 
         k = growth_coefficient, 
         max_obs_length = length_max) 

library(FishLife)
fl <- FishBase_and_Morphometrics$beta_gv

species_names <- row.names(fl)

fl <- as.data.frame(fl) %>% 
  janitor::clean_names() %>%
  mutate(species = species_names, .before = log_age_max) %>%
  as_tibble() %>%
  select(species, trophic_level, 
         temperature, log_fecundity, 
         log_growth_coefficient, log_length_max, 
         log_length_infinity, log_length_maturity, 
         log_age_maturity, log_natural_mortality, 
         log_weight_infinity, log_offspring_size) %>%
  mutate(across(log_fecundity:log_offspring_size, ~exp(.x))) %>%
  rename(fecundity = log_fecundity, 
         k = log_growth_coefficient, 
         max_obs_length = log_length_max, 
         l_inf = log_length_infinity, 
         length_maturity = log_length_maturity, 
         age_maturity = log_age_maturity, 
         natural_mortality = log_natural_mortality, 
         w_inf = log_weight_infinity,
         offspring_size = log_offspring_size)


# Write a function that takes the Beukoff database, and fills it with FishLife estimates for all trait values that were inferred from the genus or higher levels. 

extract_traits <- function(species){
  
  species_slot <- list()
  
  not_in_either <- species_list[!species_list %in% c(unique(beuk$species), unique(fl$species))]
  not_in_fishlife_in_beuk <- species_list[!species_list %in% c(unique(fl$species), not_in_either)]
  not_in_beuk_in_fishlife <- species_list[!species_list %in% c(unique(beuk$species), not_in_either)]
  

  for(i in 1:length(species)){
    
    sp_beuk = beuk[beuk$species == species[i], ]  
    sp_fl = fl[fl$species == species[i], ]
    
  if(species[i] %in% not_in_fishlife_in_beuk == T){
    species_slot[[i]] = sp_beuk %>% 
      mutate(temperature = NA,
             reference_temperature = NA,
             w_inf = NA, 
             reference_w_inf = NA, 
             natural_mortality = NA,
             reference_mortality = NA,
             length_maturity = NA,
             reference_length_maturity = NA
      )
  }else{
    if(species[i] %in% not_in_beuk_in_fishlife == T){
      fill_df <- as.data.frame(matrix(nrow = 1, ncol = length(names(beuk)))) 
      names(fill_df) <- names(beuk)
      
      species_slot[[i]] <- fill_df %>%
        mutate(species = sp_fl$species, 
               trophic_level = sp_fl$trophic_level, 
               reference_tl = "FishLife", 
               offspring_size = sp_fl$offspring_size, 
               reference_offspring_size = sp_fl$offspring_size, 
               age_maturity = sp_fl$age_maturity, 
               reference_age_maturity = "FishLife", 
               fecundity = sp_fl$fecundity, 
               reference_fecundity = "FishLife", 
               l_inf = sp_fl$l_inf, 
               k = sp_fl$k, 
               reference_growth = "FishLife", 
               max_obs_length = sp_fl$max_obs_length, 
               reference_length_max = "FishLife", 
               temperature = sp_fl$temperature,
               reference_temperature = "FishLife",
               w_inf = sp_fl$w_inf, 
               reference_w_inf = sp_fl$w_inf, 
               natural_mortality = sp_fl$natural_mortality,
               reference_mortality = "FishLife",
               length_maturity = sp_fl$length_maturity,
               reference_length_maturity = "FishLife"
        )
    }else{
      if(species[i] %in% unique(beuk$species) & species[i] %in% unique(fl$species) == T){
        if(any(sp_beuk$level_tl != "spe")){
          sp_beuk$trophic_level = sp_fl$trophic_level
          sp_beuk$reference_tl = "FishLife"
        }
        if(any(!sp_beuk$level_age_maturity %in% c("spe-ocean", "spe", "spe-FAO", "spe-LME"))){
          sp_beuk$age_maturity = sp_fl$age_maturity
          sp_beuk$reference_age_maturity = "FishLife"
        }
        if(any(!sp_beuk$level_fecundity %in% c("spe-ocean", "spe", "spe-FAO", "spe-LME"))){
          sp_beuk$fecundity = sp_fl$fecundity
          sp_beuk$reference_fecundity = "FishLife"
        }
        if(any(!sp_beuk$level_growth %in% c("spe-ocean", "spe", "spe-FAO", "spe-LME"))){
          sp_beuk$l_inf = sp_fl$l_inf
          sp_beuk$k = sp_fl$k
          sp_beuk$reference_growth = "FishLife"
        }
        if(any(!sp_beuk$level_length_max %in% c("spe-ocean", "spe", "spe-FAO", "spe-LME"))){
          sp_beuk$max_obs_length = sp_fl$max_obs_length
          sp_beuk$reference_length_max = "FishLife"
        }
        if(any(!sp_beuk$level_offspring_size %in% c("spe-ocean", "spe", "spe-FAO", "spe-LME"))){
          sp_beuk$offspring_size = sp_fl$offspring_size
          sp_beuk$reference_offspring_size = "FishLife"
        }
        species_slot[[i]] <- sp_beuk %>% 
          mutate(temperature = sp_fl$temperature,
                 reference_temperature = "FishLife",
                 w_inf = sp_fl$w_inf, 
                 reference_w_inf = sp_fl$w_inf, 
                 natural_mortality = sp_fl$natural_mortality,
                 reference_mortality = "FishLife",
                 length_maturity = sp_fl$length_maturity,
                 reference_length_maturity = "FishLife"
          )
      }
    }
  }
  }
  do.call(rbind, species_slot)
}


trait_db <- extract_traits(species_list) # This list represents all observations at the species level. 


# Extract estimates for unidentified species, only identified to the genus, family, or order level. 

need_traits <- species_df %>% 
  filter(is.na(species)) %>%
  separate(NOAA_label, into = c("taxa", "junk"), sep = " ", remove = F) %>%
  select(svspp, NOAA_label, taxa)

temp <- fl[fl$species %in% need_traits$taxa, ] # The FishLife data set contains estimates at higher level of organization (labeled as species). 


needed_traits <- as.data.frame(matrix(nrow = length(temp$species), ncol = length(names(beuk))+1)) 
names(needed_traits) <- c(names(beuk), "taxa")

needed_traits <- needed_traits %>%
  mutate(species = NA, 
         trophic_level = temp$trophic_level, 
         reference_tl = "FishLife", 
         offspring_size = temp$offspring_size, 
         reference_offspring_size = "FishLife", 
         age_maturity = temp$age_maturity, 
         reference_age_maturity = "FishLife", 
         fecundity = temp$fecundity, 
         reference_fecundity = "FishLife", 
         l_inf = temp$l_inf, 
         k = temp$k, 
         reference_growth = "FishLife", 
         max_obs_length = temp$max_obs_length, 
         reference_length_max = "FishLife", 
         temperature = temp$temperature,
         reference_temperature = "FishLife",
         w_inf = temp$w_inf, 
         reference_w_inf = temp$w_inf, 
         natural_mortality = temp$natural_mortality,
         reference_mortality = "FishLife",
         length_maturity = temp$length_maturity,
         reference_length_maturity = "FishLife", 
         taxa = temp$species) %>% 
  left_join(need_traits) %>% 
  relocate(c(taxa, svspp, NOAA_label), .before = family) %>%
  select(-taxa)



final_trait_db <- trait_db %>% 
  left_join(species_df %>% select(svspp, NOAA_label, species)) %>%
  relocate(c(svspp, NOAA_label), .before = family) %>%
  bind_rows(needed_traits) 


write.csv(trait_db, "Data/Derived/trait_database.csv")






  