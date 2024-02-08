library(tidyverse)
library(ecodata)


ppr # Data set for ecosystem overfishing indices (ex. Fogarty overfishing index). Indices are distributed by EPU\

comdat # Various classifications and indicators of total commercial landings and landings by class in each EPU in each year

species_groupings # Various taxonomic classfications that have historically been used by NEFSC

bottom_temp_comp # Bottom temperature from the High Resolution paper Pontavice et al. 2023 (Progress in Oceanography), but only distibuted aggregated to the EPU in each year. Can contact authors for full dataset
bottom_temp # Various indicators of bottom temperature
str(bottom_temp_seasonal_gridded) # A raster gridded file of bottom temperatures
bottom_temp_glorys # Bottom temperature from the Glorys model
seasonal_bt_anomaly_gridded # Gridded bottom temperature anomolies

chl_pp # Primary productivity and chlorophyll a concentrations at annual, monthly, or weekly time scales since 1998 aggregated to the EPU

species_dist

chl_pp %>% 
  filter(Var == "WEEKLY_PPD_MEDIAN") %>% 
  separate(Time, into = c("junk", "time_point"), sep = "_") %>%
  mutate(time_point = as.numeric(time_point)) %>%
  ggplot(aes(x = time_point, y = Value))+
  geom_line()+
  facet_grid(~ EPU)

ppr %>% 
  ggplot(aes(x = Time, y = Value))+
  geom_line()+
  facet_grid(Var ~ EPU, scales = "free")

p1 <- ppr %>%
  filter(Var == "Fogarty") %>% 
  ggplot(aes(x = Time, y = Value))+
  geom_line()+
  facet_wrap(~forcats::fct_reorder(EPU, Value, mean))

p2 <- comdat %>% 
  filter(Var == "Landings", EPU %in% c("MAB", "GB", "GOM")) %>% 
  ggplot(aes(x = Time, y = Value))+
  geom_line()+
  facet_wrap(~forcats::fct_reorder(EPU, Value, mean))

