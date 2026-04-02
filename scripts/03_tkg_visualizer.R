# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 03: TKG Phenotypic Visualizer
# SYSTEM ARCHITECT: Isra | BIOCURADURÍA LEAD: Layla
# ==============================================================================

# --- 1. DATA LOADING ---
csv_path <- "data/TKG_Evidence_Annotation_N-Obs.csv"

if (file.exists(csv_path)) {
  annotation_db <- read.csv(csv_path, stringsAsFactors = FALSE)
} else {
  stop("Annotation matrix not found. Please ensure the CSV is in the /data folder.")
}

# --- 2. THE VISUALIZER FUNCTION ---
inspect_species_evidence <- function(target_species, max_photos = 5) {
  
  # Filter species
  species_evidence <- annotation_db %>% 
    filter(scientific_name == target_species) %>%
    distinct(id, .keep_all = TRUE)
  
  if (nrow(species_evidence) == 0) {
    message("Species '", target_species, "' not found in the dataset.")
    return(invisible(NULL))
  }
  
  # Browser automation
  urls_to_open <- head(species_evidence$image_url, max_photos)
  cat("\n>>> Opening", length(urls_to_open), "images for:", target_species, "\n")
  
  for (url in urls_to_open) {
    utils::browseURL(url) 
    Sys.sleep(0.5) 
  }
  
  cat(">>> Tabs opened. Follow the manual in Appendix A for curation.\n")
}

# --- 3. USAGE ---
cat("\n======================================================\n")
cat("TKG VISUALIZER READY\n")
cat("======================================================\n")
cat("Example: inspect_species_evidence('Cynanthus latirostris')\n")
cat("======================================================\n")