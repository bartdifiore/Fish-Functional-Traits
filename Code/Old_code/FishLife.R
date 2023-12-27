Predictions = Plot_taxa( Search_species(Genus="Lutjanus",Species="campechanus")$match_taxonomy )
str(Predictions)


search_FishLife <- function(Genus = "Lutjanus", Species = "campechanus"){
  species_locator <- Search_species(Genus=Genus,Species=Species)
  
  predictions = Plot_taxa( species_locator$match_taxonomy )
  taxa.resolution = length(species_locator$match_taxonomy)
  
  predictions[[taxa.resolution]]$Mean_pred
  
}

search_FishLife(Genus="Lutjanus",Species="campechanus")


df <- FishBase_and_Morphometrics$beta_gv
species_names <- row.names(df)

df <- as.data.frame(df) %>% 
  janitor::clean_names() %>%
  mutate(species = species_names, .before = log_age_max) %>%
  as_tibble()

FishBase_and_Morphometrics$ParentChild_gz

sum(c(0.07785772, 0.6733307, 0.2488116))
