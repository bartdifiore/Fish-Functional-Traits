# Bring in Stat regions for aggregation

library(sf)

stat <- sf::read_sf("Data/Spatial/Statistical_Areas-selected/Statistical_Areas_2010_withNames.shp") %>%
  st_make_valid()

pj_crs <- st_crs(stat)

plot(stat["geometry"])

stat$FULL_NAME[st_is_valid(stat)==F]

stat <- stat %>%
  filter(Id != 706)


# Get bottom temperature data

library(tidync)
library(tidyverse)

bt <- tidync(x = "Data/Temperature_nc/bottom_temp_combined_product_1959_2020.nc")

temp <- bt %>%
  #hyper_filter(day = (day >91.5 & day < 91.5*2) | day >=91.5*3) %>%
  hyper_filter(year = year == 2018, 
               day = day == 244) %>%
  hyper_tibble() %>%
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  st_transform(crs = pj_crs) %>%
  st_intersection(stat)
  mutate(stat_area = st_intersection(stat))
  st_intersects(stat)
  summarize(mean_spring_temp = mean(sea_water_temperature_at_sea_floor))

temp %>% hyper_tibble()

temp %>% st_make_valid() %>%
  st_intersection(stat)
