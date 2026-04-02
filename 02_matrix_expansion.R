# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 02: High-Throughput Matrix Expansion (Arizmendi Segmentation)
# SYSTEM ARCHITECT: Isra | BIOCURADURÍA LEAD: Layla
# ==============================================================================

library(dplyr)
library(tidyr)

# --- 1. DATA LOADING ---
rds_path <- "data/tkg_hummingbirds_research_grade.rds"
if (file.exists(rds_path)) {
    final_inat_data <- readRDS(rds_path)
} else {
    stop("Data file not found. Run Script 01 first.")
}

# --- 2. ARIZMENDI ANATOMICAL MODEL (14 UNITS - UPDATED) ---
# Estos QIDs coinciden con tu Tabla 2 y el modelo de 14 regiones de México/América
anatomical_regions <- data.frame(
  region = c("Gorget", "Crest", "Nape", "Crown", "Bill", "Breast", "Flanks", 
             "Rectrices", "Rump", "Remiges", "Chin", "Belly", "Tail coverts", "Back"),
  region_qid = c("Q5586271", "Q1589150", "Q374727", "Q3321195", "Q31528", "Q21342622", "Q1427103", 
                 "Q475059", "Q1790261", "Q1433997", "Q82714", "Q3429717", "Q3178731", "Q2602751"),
  stringsAsFactors = FALSE
)

# --- 3. MATRIX EXPANSION ---
# Expandimos cada observación de iNaturalist por las 14 regiones de Arizmendi
tkg_annotation_matrix <- final_inat_data %>%
  select(scientific_name, id, image_url) %>% 
  crossing(anatomical_regions) %>%
  mutate(
    is_visible = TRUE,
    sex_P21 = NA,             # Identifier for sexual dimorphism/dichromatism (Male: Q44148 / Female: Q43445)
    wikidata_color_Q = NA,    # Wikidata QID for the primary plumage color (e.g., Green: Q3133)
    optical_property = NA,    # Structural color properties such as Iridescence (Q957208)
    confidence_score = 1.0,   # Initial data quality metric or manual verification score
    annotator_metadata = "Layla",
    timestamp = Sys.time()
  ) %>%
  arrange(scientific_name, id, region)

# --- 4. EXPORT ---
write.csv(tkg_annotation_matrix, "data/TKG_Evidence_Annotation_N-Obs.csv", row.names = FALSE)
cat("Expansion Complete.", nrow(tkg_annotation_matrix), "annotation points generated for all America.\n")
