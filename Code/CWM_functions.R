

CWM <- function(trait_vec, species_vec){
  # Data are collected at the level of the trawl. Here, I estimate the CWM for each trait, k, as the sum of the value for each trait multiplied by the biomass the species, such that
  
    cwm_trait = sum(trait_vec*species_vec)
    
}