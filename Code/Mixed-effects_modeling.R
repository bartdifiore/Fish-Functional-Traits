library(tidyverse)
library(lme4)
library(lmerTest)
source("Code/theme.R")
#-----------------------------
## Read in the data
#-----------------------------

df <- read.csv("Data/Derived/CWM_dataset.csv") %>% 
  as_tibble() %>%
  mutate(season.fct = as.factor(season), 
         ts_id = as.factor(paste(stratum, season, tow, sep = "-"))) %>% 
  arrange(stratum, tow, season, est_year)


#-----------------------------
## Build some simple models
#-----------------------------

# Trophic level
lm1 <- lm(trophic_level ~ est_year*survey_area*season + bottemp, data = df)
summary(lm1)
predict.lm1 <- ggeffects::ggpredict(lm1, terms = ~est_year*season*survey_area )
plot(predict.lm1, add.data = T)
acf(residuals(lm1))

gls1 <- nlme::gls(trophic_level~ est_year*survey_area*season, data = df, 
                  correlation = nlme::corAR1(form = ~ est_year | ts_id))
summary(gls1)
acf(residuals(gls1))
predict.gls1 <- ggeffects::ggeffect(gls1, terms = ~est_year*season*survey_area )
plot(predict.gls1)
predict.gls1 <- as.data.frame(predict.gls1) %>%
  rename(est_year = x, survey_area = facet, season = group)

df %>% 
  group_by(est_year, survey_area, season) %>% 
  summarize(mean_tl = mean(trophic_level)) %>%
  ggplot(aes( x= est_year, y = mean_tl))+
  geom_line(aes(color = season), alpha = 0.5)+
  geom_line(data = predict.gls1, aes(x = est_year, y = predicted, color = season), linewidth = 1.5)+
  geom_ribbon(data = predict.gls1, aes(x = est_year, y = predicted, ymin = conf.low, ymax = conf.high, group = season), color = NA, fill = "black", alpha = 0.25)+
  facet_wrap(~survey_area)+
  labs(x = "", y = "Trophic level")+
  theme_bd()

ggsave("Figures/trophic_univariate.png")


AIC(lm1, gls1)

gls2 <- nlme::gls(trophic_level~ survey_area*season, data = df, correlation = nlme::corAR1())
summary(gls2)
predict.gls2 <- ggeffects::ggeffect(gls2, terms = ~survey_area*season )
plot(predict.gls2)

AIC(lm1, gls1)


df$year.factor <- as.factor(df$est_year)
lmer1 <- lmerTest::lmer(trophic_level ~  est_year*survey_area + season + bottemp + (1|year.factor) + (1|svvessel), data = df)
summary(lmer1)
res.lmer1 <- DHARMa::simulateResiduals(lmer1)
plot(res.lmer1)
hist(res.lmer1)

predict.lmer1 <- ggeffects::ggpredict(lmer1, terms = ~est_year*survey_area )
plot(predict.lmer1)

AIC(lm1, lmer1)

# Length at maturity

lmer2 <- lmerTest::lmer(length_maturity ~  est_year*survey_area + season + bottemp + (1|year.factor) + (1|svvessel), data = df)
summary(lmer2)
res.lmer2 <- DHARMa::simulateResiduals(lmer1)
plot(res.lmer2)
hist(res.lmer2)

predict.lmer2 <- ggeffects::ggpredict(lmer2, terms = ~est_year*survey_area )
plot(predict.lmer2)


gls.lm <- nlme::gls(length_maturity~ est_year*survey_area*season, data = df, 
                  correlation = nlme::corAR1(form = ~ est_year | ts_id))
summary(gls.lm)
acf(residuals(gls.lm))
predict.gls.lm <- ggeffects::ggeffect(gls.lm, terms = ~est_year*season*survey_area )
plot(predict.gls.lm)

predict.gls.lm <- as.data.frame(predict.gls.lm) %>%
  rename(est_year = x, survey_area = facet, season = group)

df %>% 
  group_by(est_year, survey_area, season) %>% 
  summarize(mean_lm = mean(length_maturity)) %>%
  ggplot(aes( x= est_year, y = mean_lm))+
  geom_line(aes(color = season), alpha = 0.5)+
  geom_line(data = predict.gls.lm, aes(x = est_year, y = predicted, color = season), linewidth = 1.5)+
  geom_ribbon(data = predict.gls.lm, aes(x = est_year, y = predicted, ymin = conf.low, ymax = conf.high, group = season), color = NA, fill = "black", alpha = 0.25)+
  facet_wrap(~survey_area)+
  labs(x = "", y = "Length at maturity")+
  theme_bd()

ggsave("Figures/trophic_univariate.png")






