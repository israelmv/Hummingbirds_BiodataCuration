# ==============================================================================
# TOOL: TKG Phenotypic Visualizer (Phase 3)
# Project: Trochilidae Knowledge Graph (TKG)
# ==============================================================================

library(dplyr)

# LOAD MATRIX
annotation_db <- read.csv("data/TKG_Evidence_Annotation_N-Obs.csv", stringsAsFactors = FALSE)

# VISUALIZER FUNCTION
inspect_species_evidence <- function(target_species, max_photos = 5) {
  species_evidence <- annotation_db %>% 
    filter(scientific_name == target_species) %>%
    distinct(id, .keep_all = TRUE)
  
  if (nrow(species_evidence) == 0) return("Species not found.")
  
  urls_to_open <- head(species_evidence$image_url, max_photos)
  for (url in urls_to_open) {
    browseURL(url)
    Sys.sleep(0.5)
  }
  cat("Review complete for:", target_species, "\n")
}

# Example:
# inspect_species_evidence("Cynanthus latirostris")