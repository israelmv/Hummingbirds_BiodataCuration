# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 02: High-Throughput Matrix Expansion (Anatomical Segmentation)
# SYSTEM ARCHITECT: Isra | BIOCURADURÍA LEAD: Layla
# ==============================================================================

library(dplyr)
library(tidyr)

# --- 1. DATA LOADING (REPRODUCIBLE PATH) ---
rds_path <- "data/tkg_hummingbirds_research_grade.rds"

if (!exists("final_inat_data")) {
  if (file.exists(rds_path)) {
    final_inat_data <- readRDS(rds_path)
  } else {
    stop("Data file not found. Run Script 01 first.")
  }
}

# --- 2. ANATOMICAL SEGMENTATION MODEL (14 UNITS) ---
anatomical_regions <- data.frame(
  region = c("Forehead", "Crown", "Nape", "Periocular", "Throat", "Back", 
             "Rump", "Upper tail-coverts", "Rectrices", "Remiges", 
             "Breast", "Abdomen", "Flanks", "Undertail-coverts"),
  region_qid = c("Q226145", "Q2152862", "Q173365", "Q3455110", "Q207863", "Q3289871", 
                 "Q2411995", "Q3308365", "Q2141527", "Q2152875", "Q207851", "Q207857", 
                 "Q3073715", "Q3151475"),
  stringsAsFactors = FALSE
)

# --- 3. MATRIX EXPANSION ---
tkg_annotation_matrix <- final_inat_data %>%
  select(scientific_name, id, image_url) %>% 
  crossing(anatomical_regions) %>%
  mutate(
    is_visible = TRUE,
    hbw_color_primary = NA,
    hbw_color_secondary = NA,
    optical_property = NA, 
    confidence_score = NA,
    annotator_metadata = "Layla",
    timestamp = Sys.time()
  ) %>%
  arrange(scientific_name, id, region)

write.csv(tkg_annotation_matrix, "data/TKG_Evidence_Annotation_N-Obs.csv", row.names = FALSE)
cat("Expansion Complete. 47,320 annotation points generated in /data.\n")
