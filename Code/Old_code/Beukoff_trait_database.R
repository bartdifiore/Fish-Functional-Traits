library(tidyverse)

beuk <- read.csv("Data/Other_trait_databases/Beukoff_2019.csv")

fb <- read.csv("Data/Derived/trait_database.csv")
sp_fb <- as.vector(unique(fb$species))
sp_beuk <- as.vector(unique(paste(beuk$genus, beuk$species, sep = " ")))


sp_fb[sp_fb %in% sp_beuk]
sp_fb[!sp_fb %in% sp_beuk]

bk <- beuk %>% 
  janitor::clean_names()
