# Bring in Stat regions for aggregation

library(sf)
library(tidyverse)

stat <- sf::read_sf("Data/Spatial/Statistical_Areas-selected/Statistical_Areas_2010_withNames.shp") %>%
  st_make_valid()

pj_crs <- st_crs(stat)

plot(stat["geometry"])

stat$Id[st_is_valid(stat)==F]

stat <- stat %>%
  filter(Id != 706) # Get rid of the one geometry that is invalid.


# Get bottom temperature data

library(tidync)


bt <- tidync(x = "Data/Temperature_nc/bottom_temp_combined_product_1959_2020.nc")

system.time({aggregate_temp <- bt %>%
  hyper_filter(day = (day >91.5 & day < 91.5*2) | day >=91.5*3) %>%
  # hyper_filter(year = year == 2018, 
  #              day = day == 92) %>%
  hyper_tibble() %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  st_transform(crs = pj_crs) %>%
  st_join(stat) %>%
  st_drop_geometry() %>%
  drop_na(Id) %>%
  mutate(season = case_when(day < 91.5*2 ~ "spring", 
                            day >= 91.5*3 ~ "fall")) %>%
  group_by(year, season, Id) %>%
  summarize(mean_temp = mean(sea_water_temperature_at_sea_floor, na.rm = T), 
            sd_temp = sd(sea_water_temperature_at_sea_floor, na.rm = T))})



write.csv(aggregate_temp, "Data/Derived/bottom_temp_stat_area.csv", row.names = F, quote = F)

ggplot(aggregate_temp, aes(x = year, y = mean_temp))+
  geom_line(aes(group = Id))+
  facet_wrap(~season)
ggplot(aggregate_temp, aes(x = mean_temp, y = sd_temp))+
  geom_point()
ggplot(aggregate_temp, aes(x = mean_temp, y = mean_temp/sd_temp))+
  geom_point()


