# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 01: Taxonomic Alignment and Data Harvesting
# SYSTEM ARCHITECT: Isra | BIOCURADURÍA LEAD: Layla
# ==============================================================================

# --- 1. DEPENDENCY CHECK ---
req_pkgs <- c("taxize", "tidyverse", "rgbif", "rinat", "dplyr")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
library(taxize); library(tidyverse); library(rgbif); library(rinat)

# --- 2. ENVIRONMENT SETUP ---
if(!dir.exists("data")) dir.create("data")

# --- 3. TAXONOMIC BACKBONE ---
target_family <- "Trochilidae"
check_name <- name_backbone(name = target_family, rank = "family")
if(is.null(check_name$usageKey)) stop("Family 'Trochilidae' not found in GBIF Backbone.")

# Harvest Accepted Species (GBIF ID: 5289)
species_taxonomy <- name_lookup(higherTaxonKey = 5289, rank = "SPECIES", limit = 1000)
master_list <- species_taxonomy$data %>%
  filter(taxonomicStatus == "ACCEPTED") %>%
  select(scientificName, canonicalName, key) %>%
  distinct(canonicalName, .keep_all = TRUE)

write.csv(master_list, "data/Trochilidae_Master_List.csv", row.names = FALSE)

# --- 4. iNATURALIST HARVESTING ---
all_observations <- list()
for(i in 1:nrow(master_list)){
  species_name <- master_list$canonicalName[i]
  message(paste("Harvesting:", species_name, "(", i, "/", nrow(master_list), ")"))
  
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
  Sys.sleep(0.5) # Protect IP from rate-limiting
}

# --- 5. CONSOLIDATION ---
final_inat_data <- bind_rows(all_observations)
saveRDS(final_inat_data, "data/tkg_hummingbirds_research_grade.rds")
write.csv(final_inat_data, "data/tkg_hummingbirds_research_grade.csv", row.names = FALSE)

cat("\nDone. Records saved in /data folder.\n")