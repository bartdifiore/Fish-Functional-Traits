# Libraries

library(tidyverse)
library(rfishbase)


# Build out the species list

df <- read.csv("Data/NMFS_trawl/NMFS_survdat_gmri_tidy.csv") # Trawl survey data...

svspp_vec <- unique(df$svspp)

species_list <- read.csv("Data/NMFS_trawl/svspp_spp names.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(svspp, sciname) %>%
  filter(svspp %in% svspp_vec) %>%
  mutate(Sciname = stringr::str_to_sentence(sciname), 
         Sciname = case_when(Sciname == "Urophycis earlli" ~ "Urophycis earllii", 
                             Sciname == "Aluterus heudeloti" ~ "Aluterus heudelotii", 
                             .default = Sciname))

species_vec <- as.vector(species_list$Sciname)

species_list$validated_species <- rfishbase::validate_names(species_list$Sciname) # This compared the species list to the Fishbase list and validates species that are in the data base or that have alternate scientfic names in the data base.

# The SeaLife data base has information on other species (e.g. not fish). So here, I push the list of unvalidated species against the SeaLife data base. 
non_fish <- load_taxa(server = "sealifebase") %>%
  filter(Species %in% species_list$Sciname)

species_to_remove <- c(non_fish$Species, "Unknown 04", "Unknown 05", "Unknown 06", "Unidentified fish", "No fish but good tow", "Chlamys islandica clapper", "Placopecten magellanicus clapper", "Loligo pealeii", "Loligo pleii", "Geryon quinquedens", "Geryon fenneri") # Get rid of the nonfish, unknown species codes, the good tow code, two mollusk species codes for clappers (empty shells), two squid species, and two crab species.)

taxonomy <- load_taxa() %>%
  janitor::clean_names() %>%
  select(-spec_code)


species_list2 <- species_list %>% 
  filter(!Sciname %in% species_to_remove) %>%
  separate(Sciname, into = c("unclassified", "junk"), sep = " ", remove = F) %>%
  mutate(unclassified = ifelse(unclassified == "Paralepidae", "Paralepididae", unclassified)) %>% # One of the genera was spelt incorrectly in the dataset. 
  left_join(taxonomy, join_by(validated_species == species))


still_not_validated <- species_list2 %>% filter(is.na(validated_species) == T) %>%
  select(Sciname, svspp)
still_not_validated_Sciname <- as.vector(still_not_validated$Sciname)
still_not_validated_svspp <- as.vector(still_not_validated$svspp)


hellish <- function(name){
  if(name %in% taxonomy$genus == T){
    pull = unique(taxonomy[taxonomy$genus == name, -1])[1,]
    data.frame(species = NA, pull)
  }else{
    if(name %in% taxonomy$family == T){
      pull = unique(taxonomy[taxonomy$family == name, -c(1,2,3)])[1,]
      data.frame(species = NA, genus = NA, subfamily = NA, pull)
    }else{
      if(name %in% taxonomy$order == T){
        pull = unique(taxonomy[taxonomy$order == name, -c(1:4)])[1,]
        data.frame(species = NA, genus = NA, subfamily = NA, family = NA, pull)
      }else{
        data.frame(species = NA, genus = NA, subfamily = NA, family = NA, order = NA, class = NA, super_class = NA)
      }
    }
  }
}

out <- list()
for(i in 1:length(still_not_validated_unclassified)){
  out[[i]] <- hellish(still_not_validated_unclassified[i])
}

formerge <- do.call(rbind, out) %>%
  mutate(Sciname = still_not_validated_Sciname, 
         svspp = still_not_validated_svspp, 
         validated_species = NA) %>%
  select(svspp, Sciname, validated_species, genus, subfamily, family, order, class, super_class) %>%
  drop_na(super_class) # Dump the species that aren't fish
  

final_list <- species_list2 %>%
  drop_na(validated_species) %>%
  select(-c(sciname, unclassified, junk)) %>%
  bind_rows(formerge) %>%
  rename(NOAA_label = Sciname)


write.csv(final_list, "Data/Derived/species_dataframe.csv", row.names = F, quote = F)
