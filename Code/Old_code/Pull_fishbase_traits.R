
library(rfishbase)
library(tidyverse)



docs("species") # The species table seems to be the fastest method to get most of the necessary info from fishbase. However, there may be more detailed info contained in the other tables, which could be queried independently. 
# View(docs("popgrowth"))
# View(docs("popqb"))
# docs("morphdat")
# docs("morphmet")
# docs("fecundity")
# docs("taxa")


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

validated_species <- rfishbase::validate_names(species_vec) # This compared the species list to the Fishbase list and validates species that are in the data base or that have alternate scientfic names in the data base.

temp.df <- data.frame(species_vec, validated_species) %>% # There are issues because not all species in the NOAA trawl survey data are fish (e.g. crabs, lobster, etc.). Furthermore, in many instances fish are only identified to family or genus.
  replace_na(list(validated_species = "unknown")) # Make an easier variable to subset

not_validated <- filter(temp.df, validated_species == "unknown")
not_validated <- as.vector(not_validated$species_vec) # Vector of species that need to be dealt with

# The SeaLife data base has information on other species (e.g. not fish). So here, I push the list of unvalidated species against the SeaLife data base. 
non_fish <- load_taxa(server = "sealifebase") %>%
  filter(Species %in% not_validated)

still_not_validated <- not_validated[!not_validated %in% non_fish$Species] # Exclude the non-fish species from the list

still_not_validated <- still_not_validated[!still_not_validated %in% c("Unknown 04", "Unknown 05", "Unknown 06", "Unidentified fish", "No fish but good tow", "Chlamys islandica clapper", "Placopecten magellanicus clapper", "Loligo pealeii", "Loligo pleii", "Geryon quinquedens", "Geryon fenneri")] # Get rid of the unknown species codes, the good tow code, two mollusk species codes for clappers (empty shells), two squid species, and two crab species.


genera <- separate(as.data.frame(still_not_validated), still_not_validated, into = c("genus", "junk"), sep = " ") %>%
  select(-junk) # This is the remaining list of genera, where individiduals were not identified to species level. 

validated_highertaxa <- as.vector(genera$genus)
validated_highertaxa <- ifelse(validated_highertaxa == "Paralepidae", "Paralepididae", validated_highertaxa) # One of the genera was spelt incorrectly in the dataset. 

taxonomic <- load_taxa() %>%
  filter(Species %in% validated_species)

higher_order1 <- load_taxa() %>%
  distinct(Genus, Family, Order, Class) %>%
  filter(Genus %in% validated_highertaxa)

higher_order2 <- load_taxa() %>%
  distinct(Family, Order, Class) %>%
  filter(Family %in% validated_highertaxa)

higher_order3 <- load_taxa() %>%
  distinct(Order, Class) %>%
  filter(Order %in% validated_highertaxa)

validated_highertaxa[!validated_highertaxa %in% c(higher_order1$Genus, higher_order2$Family, higher_order3$Order)] # all of these species are NOT ray finned fish. Selachimorpha are sharks (not rays), but it doesn't appear that fishbase recognizes this taxonomic designation.

species_df <- taxonomic %>%
  bind_rows(higher_order1) %>% 
  bind_rows(higher_order2) %>%
  bind_rows(higher_order3) %>% # Organize a species data frame with rows for species only identified to genus, family, or order.
  janitor::clean_names()

write.csv(species_df, "Data/Derived/species_dataframe.csv", row.names = F, quote = F)

fish_vec <- as.vector(species_df$species)
Speccode_vec <- as.vector(species_df$spec_code)

species_tbl <- species(species_list = fish_vec) %>% #The species table is the most common way of extracting data related to species. The species table is NOT organized by stocks and to my knowledge all traights (e.g. life history parameters) at the species-level are selected as the maximum across the individuals stocks of that species. 
  janitor::clean_names() %>%
  left_join(species_df) %>% 
  select(species, spec_code, genus, demers_pelag, ana_cat, depth_range_shallow, depth_range_deep, depth_range_com_shallow, depth_range_com_deep, longevity_wild, vulnerability, length, l_type_max_m, common_length, l_type_com_m, weight, importance, family, order, class, super_class)

# Each of the following tables is organized by stocks. Thus there are (potentially) multiple records for each species. Moreover, there are multiple records of each variables within a stock. For example, for length at first maturity, FishBase may have different estimates from different references for each stock. 

maturity_tbl <- fb_tbl(tbl = "maturity") %>%
  filter(Speccode %in% Speccode_vec) %>% 
  janitor::clean_names() %>%
  rename(spec_code = speccode) %>%
  left_join(species_df) %>%
  select(spec_code, stock_code, species, tm, lm) %>% 
  group_by(spec_code, stock_code, species) %>%
  summarize(across(tm:lm, ~mean(.x, na.rm = T))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  filter(!if_all(tm:lm, ~ is.na(.x)))

        # This is a function to deal with estimating the midpoint for two values, or alternately selecting the value that is present.
        midpoint <- function(x,y){
          ifelse(is.na(x), y, 
                 ifelse(is.na(y), x, (x+y)/2))
        }
        
        temp <- data.frame(x = c(1, NA, 1), y = c(NA, 1, 2))
        
        midpoint(x = temp$x, y = temp$y)

fecundity_tbl <- fb_tbl(tbl = "fecundity") %>%
  filter(SpecCode %in% Speccode_vec) %>%
  janitor::clean_names() %>%
  left_join(species_df) %>%
  select(stock_code, spec_code, species, fecundity_min, fecundity_max) %>%  #absolute # of oocystes 
  group_by(spec_code, stock_code, species) %>%
  summarize(across(fecundity_min:fecundity_max, ~mean(.x, na.rm = T))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  filter(!if_all(fecundity_min:fecundity_max, ~ is.na(.x))) %>%
  mutate(fecundity_mean = midpoint(x = fecundity_min, y = fecundity_max)) %>% 
  select(spec_code, stock_code, species, fecundity_mean)

# oxygen_tbl <- fb_tbl(tbl = "oxygen") %>%
#   filter(SpecCode %in% Speccode_vec) %>%
#   janitor::clean_names() %>%
#   left_join(species_df)

ecology_tbl <- fb_tbl(tbl = "ecology") %>%
  filter(SpecCode %in% Speccode_vec) %>%
  janitor::clean_names() %>%
  drop_na(spec_code) %>%
  left_join(species_df) %>% 
  select(stock_code, spec_code, species, herbivory2, feeding_type, diet_troph, food_troph, schooling) %>% # Only one species (Atlantic sturgeon) has multiple stocks. 
  mutate(trophic_num = ifelse(is.na(diet_troph), food_troph, diet_troph)) %>% 
  select(-c(diet_troph, food_troph))
 

popchar_tbl <- fb_tbl(tbl = "popchar") %>%
  filter(Speccode %in% Speccode_vec) %>% 
  janitor::clean_names() %>%
  rename(spec_code = speccode) %>%
  left_join(species_df) %>%
  select(spec_code, stockcode, species, wmax, lmax, tmax) %>%
  rename(stock_code = stockcode) %>%
  group_by(spec_code, stock_code, species) %>%
  summarize(across(wmax:tmax, ~mean(.x, na.rm = T))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  filter(!if_all(wmax:tmax, ~ is.na(.x)))

popgrowth_tbl <- fb_tbl(tbl = "popgrowth") %>%
  filter(SpecCode %in% Speccode_vec) %>%
  janitor::clean_names() %>%
  left_join(species_df) %>%
  select(spec_code, stock_code, species, loo, k, winfinity, tmax, rm) %>% # rm = intrinsic rate of population increase
  group_by(spec_code, stock_code, species) %>%
  summarize(across(loo:rm, ~mean(.x, na.rm = T))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  filter(!if_all(loo:rm, ~ is.na(.x)))


eggs_tbl <- fb_tbl(tbl = "eggs") %>%
  filter(Speccode %in% Speccode_vec) %>% 
  janitor::clean_names() %>%
  rename(spec_code = speccode, stock_code = stockcode) %>%
  select(spec_code, stock_code, eggdiammax, eggdiammin, eggdiammod) %>%
  group_by(spec_code, stock_code) %>%
  summarize(across(eggdiammax:eggdiammod, ~mean(.x, na.rm = T))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  filter(!if_all(eggdiammax:eggdiammod, ~ is.na(.x))) %>% 
  mutate(egg_dia = ifelse(is.na(eggdiammax) & is.na(eggdiammin), eggdiammod, midpoint(x = eggdiammax, y = eggdiammin))) %>% 
  select(-c(eggdiammax, eggdiammin, eggdiammod))


# Use this to filter by geographic location!!!! Need to finish that!

fao_areas <- faoareas() %>% 
  janitor::clean_names() %>%
  filter(area_code %in% c(21, 31)) 

NW_atlantic_stocks <- unique(fao_areas$stock_code) # This should be a list of all stocks that occur within the northwest atlantic 

# Merge stock level traits
stock_traits <- maturity_tbl %>%
  left_join(fecundity_tbl) %>%
  left_join(ecology_tbl) %>%
  left_join(popchar_tbl) %>%
  left_join(popgrowth_tbl) %>% 
  left_join(eggs_tbl)%>%
  filter(stock_code %in% NW_atlantic_stocks) # Limit all stock-specific traits to estimates for the Northwest atlantic (e.g. not the Northeast atlantic where many species names are the same but the traits may differ significantly.)

# Merge stock level traits w/ the species level
trait_db <- species_tbl %>% 
  left_join(stock_traits)

write.csv(trait_db, "Data/Derived/Fishbase_filtered.csv", quote = F, row.names = F)














#----------------------------------------------------
## Scrap code
#----------------------------------------------------

# Fish base is organized by species and stocks. 

stocks <- fb_tbl(tbl = "stocks")%>%
  janitor::clean_names() %>%
  filter(spec_code %in% Speccode_vec) %>%
  left_join(species_df)

stocks %>% 
  distinct(stock_code, species, local_unique)









distinct(fao_areas, StockCode, AreaCode)
unique(fao_areas$StockCode)

FB_species_list <- load_taxa() # So this will load the entire data base of all species and their respective taxonomy. 

table_list <- fb_tables() # this is the full list of available tables. Some of these tables can be accessed from docs(), some have no documentatio.
sort(table_list) # Display it so its easier to find what your looking for.

genera <- fb_tbl(tbl = "genera") # This table is similar to fb_tbl(tbl = "species") except organized by genera. Can use this table to pull traits when only the genus or family is recognized.

docs("genera")
docs("species")

load_taxa()









