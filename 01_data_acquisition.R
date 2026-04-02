# ==============================================================================
# STEP 01: Taxonomic Alignment and Citizen Science Data Harvesting
# Project: Trochilidae Knowledge Graph (TKG)
# ==============================================================================

library(taxize)
library(tidyverse)
library(rgbif)
library(rinat)

# 1. TAXONOMIC CHECK
target_family <- "Trochilidae"
check_name <- name_backbone(name = target_family, rank = "family")
if(is.null(check_name$usageKey)) stop("Error: Family name not found.")

# 2. HARVEST ACCEPTED SPECIES (GBIF ID: 5289)
species_taxonomy <- name_lookup(higherTaxonKey = 5289, rank = "SPECIES", limit = 1000)
master_list <- species_taxonomy$data %>%
  filter(taxonomicStatus == "ACCEPTED") %>%
  select(scientificName, canonicalName, key) %>%
  distinct(canonicalName, .keep_all = TRUE)

write.csv(master_list, "data/Trochilidae_Master_List.csv", row.names = FALSE)

# 3. HARVEST RESEARCH-GRADE EVIDENCE (iNaturalist)
all_observations <- list()
for(i in 1:nrow(master_list)){
  species_name <- master_list$canonicalName[i]
  print(paste("Harvesting:", species_name, "(", i, "/", nrow(master_list), ")"))
  
  obs <- tryCatch({ get_inat_obs(query = species_name) }, error = function(e) return(NULL))
  
  if(!is.null(obs) && is.data.frame(obs) && nrow(obs) > 0){
    obs_filtered <- obs %>%
      filter(quality_grade == "research") %>%
      head(10)
    
    if(nrow(obs_filtered) > 0){
      obs_filtered$search_name <- species_name
      all_observations[[i]] <- obs_filtered
    }
  }
  Sys.sleep(0.5) # UNAM IP Protection
}

# 4. CONSOLIDATION
final_inat_data <- bind_rows(all_observations)
saveRDS(final_inat_data, "data/tkg_hummingbirds_research_grade.rds")
write.csv(final_inat_data, "data/tkg_hummingbirds_research_grade.csv", row.names = FALSE)