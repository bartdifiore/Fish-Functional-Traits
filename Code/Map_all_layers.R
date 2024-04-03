library(tidyverse)
library(sf)


# Ecological production units:

library(ecodata)

epu <- epu_sf %>%
  st_transform(pj_crs) %>%
  select(EPU, geometry) %>%
  st_make_valid()

st_is_valid(epu)

st_bbox(epu)

# Statistical landing areas: 

stat <- sf::read_sf("Data/Spatial/Statistical_Areas-selected/Statistical_Areas_2010_withNames.shp") %>%
  st_make_valid()

pj_crs <- st_crs(stat)

plot(stat["geometry"])

stat$Id[st_is_valid(stat)==F]

stat <- stat %>%
  filter(Id != 706) %>% # Get rid of the one geometry that is invalid.
  select(Id, geometry)


# Trawl survey locations: 

df_raw <- read.csv("Data/NMFS_trawl/NMFS_survdat_gmri_tidy.csv") %>%
  distinct(decdeg_beglat, decdeg_beglon) %>% 
  mutate(tow_id = 1:n()) %>%
  st_as_sf(coords = c("decdeg_beglon", "decdeg_beglat")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  st_transform(crs = pj_crs)


# Merge EPU with stat areas

df_spatial <- stat %>% 
  st_join(epu)



# Build the map

ggplot()+
  geom_sf(data = df_raw, size = 0.1)+
  geom_sf(data = stat, fill = "transparent", color = "blue")+
  geom_sf(data = epu, fill = "transparent", color = "red")+
  coord_sf(xlim = c(-76,-66), ylim = c(35,45))


ggplot()+
  geom_sf(data = df_raw, size = 0.1)+
  geom_sf(data = df_spatial, aes(fill = EPU), alpha = 0.5, color = "blue")+
  geom_sf(data = epu, fill = "transparent", color = "red")+
  coord_sf(xlim = c(-76,-66), ylim = c(35,45))
ggsave("Figures/mapissue.png")









