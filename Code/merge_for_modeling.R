library(tidyverse)
pca <- read.csv("Data/Derived/CWM_pca_variables.csv") %>%
  rename(year = est_year) %>%
  mutate(season = tolower(season))

btemp <- read.csv("Data/Derived/bottom_temp_stat_area.csv")

landings <- read.csv("Data/Landings/Finfishlandings_bystatarea.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  rename(Id = stat_area) %>%
  group_by(year, Id) %>%
  summarize(landings = sum(landed_lbs, na.rm = T)) 


df <- pca %>%
  left_join(btemp) %>%
  complete(year, Id, season) %>%
  left_join(landings) %>%
  drop_na(Id) %>%
  mutate(cv_temp = mean_temp/sd_temp) %>%
  filter(!if_all(PC1:cv_temp, is.na)) %>%
  pivot_longer(cols = PC1:PC3, names_to = "PCA_axis", values_to = "PCA_value") %>%
  mutate(log.landings = log(landings))


library(glmmTMB)

mod1 <- glmmTMB(PCA_value ~ PCA_axis*(scale(mean_temp) + scale(cv_temp) + scale(log.landings)) + (1|year) + (1|Id), df, family = gaussian(link = "identity"))
summary(mod1)

library(DHARMa)
res <- simulateResiduals(mod1)
plot(res)

library(ggeffects)

pred1 <- ggpredict(mod1, terms = ~mean_temp*PCA_axis)
plot(pred1)

pred2 <- ggpredict(mod1, terms = ~cv_temp*PCA_axis)
plot(pred2)

pred3 <- ggpredict(mod1, terms = ~log.landings*PCA_axis, cov.fit = F)
plot(pred3)


pred.re <- mod1$frame
pred.re$estimate <- predict(mod1)

library(sf)
library(tidyverse)

stat <- sf::read_sf("Data/Spatial/Statistical_Areas-selected/Statistical_Areas_2010_withNames.shp") %>%
  st_make_valid()

pj_crs <- st_crs(stat)

plot(stat["geometry"])

stat$Id[st_is_valid(stat)==F]

stat <- stat %>%
  filter(Id != 706) %>% # Get rid of the one geometry that is invalid.
  select(Id, geometry) %>%
  right_join(pred.re)

temp <- stat %>% 
  filter(year %in% c(1985, 2018), 
         PCA_axis == "PC1") %>%
  select(estimate, geometry, year)

ggplot()+
  geom_sf(data = temp, aes(fill = estimate))+
  facet_wrap(~year)


ggplot(df, aes(x = mean_temp, y = PCA_value))+
  geom_point()+
  facet_wrap(~PCA_axis)
