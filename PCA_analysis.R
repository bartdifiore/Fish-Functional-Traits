
library(tidyverse)

source("Code/CWM_functions.R")

# Pull in the species list and trait database

trait_db <- read.csv("Data/Derived/trait_database.csv")[, -1] %>% as_tibble()


trait_mat <- trait_db %>% 
  select(svspp, trophic_level, offspring_size, age_maturity, length_maturity, fecundity, l_inf, k, max_obs_length) %>%
  drop_na(l_inf, length_maturity) %>% # One species missing l_inf. filtering to avoid NA...
  group_by(svspp) %>%
  summarize(across(where(is.double), \(x) mean(x, na.rm = T))) %>%
  mutate(across(where(is.double), \(x) log(x)))

GGally::ggpairs(trait_mat[,-1], diag = list(continuous = "barDiag"))

pc <- prcomp(trait_mat[,-1], scale. = T)
print(pc)
summary(pc)


library(ggfortify)
p1 <- autoplot(pc,
         x = 1, 
         y = 2,
         loadings = T, 
         loadings.label = T,
         loadings.label.hjust = 1.2, 
         loadings.label.vjust = -.75,
         color = "grey")+
  theme_classic()

autoplot(pc,
         x = 1, 
         y = 3,
         loadings = T, 
         loadings.label = T, 
         loadings.label.hjust = 1.2, 
         loadings.label.vjust = -.75,
         color = "grey")+
  theme_classic()

p2 <- autoplot(pc,
         x = 2, 
         y = 3,
         loadings = T, 
         loadings.label = T, 
         loadings.label.hjust = 1.2, 
         loadings.label.vjust = -.75,
         color = "grey")+
  theme_classic()

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave("Figures/pca_ordination.png", width = 10, height = 5)


pca_df <- as.data.frame(pc$x[, 1:3]) %>%
  bind_cols(trait_mat$svspp) %>%
  rename(svspp = "...4") %>%
  as_tibble()

write.csv(pca_df, "Data/Derived/pca_variables.csv", row.names = F, quote = F)







