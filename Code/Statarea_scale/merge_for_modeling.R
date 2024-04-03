library(tidyverse)
source("Code/theme.R")
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
  mutate(log.landings = log(landings), 
         year.fct = as.factor(year), 
         time = as.numeric(year), 
         stat_area = as.factor(Id))



GGally::ggpairs(df, columns = c("time", "mean_temp", "log.landings", "cv_temp"))


library(glmmTMB)

mod1 <- glmmTMB(PCA_value ~ PCA_axis*(scale(mean_temp) + scale(cv_temp) + scale(log.landings)) + season + (1|year.fct) + (mean_temp + log.landings|stat_area), df, family = gaussian(link = "identity"))
summary(mod1)

library(DHARMa)
res <- simulateResiduals(mod1)
plot(res)

library(ggeffects)

pred1 <- ggpredict(mod1, terms = ~mean_temp*PCA_axis)
plot(pred1)+
  theme_bd()
ggsave("Figures/pca_forreview.png")


as.data.frame(pred1)%>%
  rename(PCA_axis = group) %>%
  ggplot()+
  geom_point(data = df, aes(x = mean_temp, y = PCA_value, color = PCA_axis), alpha = 0.25, size = 0.25, show.legend = F)+
  geom_line(aes(x = x, y = predicted, color = PCA_axis), show.legend = F, linewidth = 2)+
  geom_ribbon(aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), alpha = 0.25, show.legend = F)+
  facet_wrap(~PCA_axis, scales = "free")+
  theme_classic()
ggsave("Figures/pca_forreview2.png")


pred2 <- ggpredict(mod1, terms = ~cv_temp*PCA_axis)
plot(pred2)

pred3 <- ggpredict(mod1, terms = ~log.landings*PCA_axis)
plot(pred3)+
  theme_bd()
ggsave("Figures/pca_forreview2.png")

pred4 <- ggpredict(mod1, terms = ~mean_temp|stat_area*PCA_axis, type = "random", interval = "confidence")
as.data.frame(pred4) %>%
  ggplot(aes(x = x, y = predicted))+
  geom_line(aes(color = facet))+
  geom_ribbon(aes(group = facet, ymax = conf.high, ymin = conf.low), alpha = 0.1)+
  facet_wrap(~group)+
  theme_classic()

pred5 <- ggpredict(mod1, terms = ~log.landings|stat_area*PCA_axis, type = "random", interval = "confidence")
as.data.frame(pred5) %>%
  ggplot(aes(x = x, y = predicted))+
  geom_line(aes(color = facet))+
  geom_ribbon(aes(group = facet, ymax = conf.high, ymin = conf.low), alpha = 0.1)+
  facet_wrap(~group)+
  theme_classic()


pred.re <- mod1$frame
pred.re$estimate <- predict(mod1)

ggplot(pred.re, aes(x = year.fct, y = estimate))+
  geom_line(aes(group = stat_area))+
  facet_wrap(season~PCA_axis)



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
  mutate(stat_area = as.factor(Id)) %>%
  right_join(pred.re)

temp <- stat %>% 
  filter(year.fct %in% c(1985, 1995, 2005, 2015), 
         season == "fall",
         PCA_axis == "PC1") %>%
  select(estimate, geometry, year.fct)

map <- ggplot()+
  geom_sf(data = temp, aes(fill = estimate))+
  facet_wrap(~year.fct)+
  labs(fill = "Pace of life (PCA1)")+
  theme_bd()
ggsave("Figures/map_stat_areas.png", map)



ggplot(df, aes(x = mean_temp, y = PCA_value))+
  geom_point()+
  facet_wrap(~PCA_axis)


ggplot(df, aes(x = year, y = landings))+
  geom_line(aes(color = season))+
  facet_wrap(~stat_area, scales = "free")

ggplot(df, aes(x = year, y = mean_temp))+
  geom_line(aes(color = season))+
  facet_wrap(~stat_area, scales = "free")

ggplot(df, aes(x = mean_temp, y = PCA_value))+
  geom_point(aes(color = season))+
  geom_smooth(aes(color = season))+
  facet_wrap(~PCA_axis, scales = "free")



library(rstanarm)

# stan1 <- rstanarm::stan_glmer(PCA_value ~ PCA_axis*(scale(mean_temp) + scale(cv_temp) + scale(log.landings)) + season + (1|year.fct) + (mean_temp + log.landings|stat_area), df, family = gaussian(link = "identity"))







