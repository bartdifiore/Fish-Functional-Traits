# Based on a non-systematic review of the literature, I selected 17 papers that focused on fish functional traits. In the dataset below, the Trait variable represents the labels that the authors assigned to each trait they used in there analysis. I manually consolodated traits that were the same but were assiged different labels in the variable, "Trait_cat". Of the 17 papers, 11 used maximum body size (either length or weight) and some categorical metric of reproductive strategy. 7/17 included trophic breadth (some meaure of diet breadth), trophic guild (e.g. predator, herbivore, etc.), and trophic level (via Fishbase). 6/17 included metrics of reproductive output (e.g. fecundity), length at first maturity, offspring size, and water column position (demersal, pelagic, etc.).


lit_review <- read.csv("Data/Lit_review/Trait_litreview.csv")
unique(lit_review$Trait_cat)
lit_review %>%
  group_by(Trait_cat) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  View()


lit_review %>%
  group_by(Trait_cat) %>%
  summarize(n = n()) %>%
  filter(n>1) %>%
  ggplot()+
  geom_bar(aes(x = forcats::fct_reorder(Trait_cat, n,.desc = T), y = n), stat = "identity")+
  labs(x = "", y = "Frequency")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("Figures/lit_review_traits.png")
