# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 04: Strict Biocentric Color Dictionary
# ==============================================================================
# This script processes the raw Wikidata export to create a curated color 
# lexicon for hummingbird phenotypic annotation. 
# It implements a rigorous filtration protocol to exclude:
# 1. Biological/Cellular processes (e.g., "pigment cell differentiation").
# 2. Commercial/Software brands (e.g., "Microsoft", "Android").
# 3. Industrial standards (e.g., "Pantone", "RAL", "ISCC").
# 4. Ghost entities (Entries where the label is just the Wikidata QID).
# ==============================================================================

library(dplyr)
library(stringr)

# --- 1. DATA LOADING ---
# Ensure query.csv is in your data/ directory
raw_wikidata <- read.csv("data/Wikidata_colors.csv", stringsAsFactors = FALSE)

# --- 2. THE "BIO-PURGE" (ULTRA-STRICT FILTRATION) ---
tkg_sanitized_colors <- raw_wikidata %>%
  # Filter 1: Remove cellular, genetic, and metabolic processes (Data Noise)
  filter(!str_detect(colorLabel, "(?i)process|cell|differentiation|granule|metabolic|developmental|hair")) %>%
  
  # Filter 2: Remove commercial, political, or software-related entities
  filter(!str_detect(colorLabel, "(?i)Microsoft|Android|Discord|UCLA|Yale|Pitufo|national|Ukraine|Google|Apple|Facebook")) %>%
  
  # Filter 3: INDUSTRIAL CLEANING (Strict exclusion of Pantone, RAL, and ISCC codes)
  # This ensures the ontology remains focused on natural phenotypic descriptors.
  filter(!str_detect(colorLabel, "(?i)Pantone|RAL [0-9]|RAL color|ISCC")) %>%
  
  # Filter 4: REMOVE GHOST ENTITIES (Labels that are just QIDs like "Q1499005")
  # This cleans entries lacking a natural language label in the primary metadata.
  filter(!str_detect(colorLabel, "^Q[0-9]+$")) %>%
  
  # Filter 5: Visual Verification (Ensure the presence of a Hexadecimal code)
  # Exceptions are made for common avian descriptors like "Sooty" or "brindle".
  filter(nchar(hex) > 0 | colorLabel %in% c("Sooty", "brindle")) %>%
  
  # Filter 6: QID Normalization
  mutate(wikidata_qid = str_extract(color, "Q[0-9]+"))

# --- 3. BIOLOGICAL PROPERTY CLASSIFICATION ---
# Automated mapping to distinguish between structural (Iridescent) and pigmentary colors.
tkg_sanitized_colors <- tkg_sanitized_colors %>%
  mutate(
    type = case_when(
      str_detect(colorLabel, "(?i)emerald|golden|sapphire|ruby|iridescent|glittering|metallic|turquoise|fiery") ~ "Iridescent",
      str_detect(colorLabel, "(?i)rufous|buff|cinnamon|sooty|brown|grey|black|white|ochre|vinaceous") ~ "Pigmentary",
      TRUE ~ "General"
    ),
    is_natural_plumage = TRUE
  ) %>%
  select(hbw_name = colorLabel, wikidata_qid, hex, type, is_natural_plumage) %>%
  distinct(wikidata_qid, .keep_all = TRUE)

# --- 4. CORE 14 INTEGRATION (Arizmendi & HBW Baseline) ---
# Hard-coded baseline to ensure consistency with the 14-region segmentation model.
core_14 <- data.frame(
  hbw_name = c("Emerald Green", "Golden Green", "Sapphire Blue", "Ruby Red", 
               "Glittering Copper", "Fiery Orange", "Violet", "Rufous", 
               "Cinnamon", "Buff", "Sooty", "White", "Black", "Iridescent Purple"),
  wikidata_qid = c("Q691510", "Q5579480", "Q108183", "Q1120984", 
                   "Q5168705", "Q1378330", "Q8063", "Q7377981", 
                   "Q1148100", "Q4985885", "Q7562417", "Q23444", "Q23445", "Q211140"),
  hex = c("50C878", "FFDF00", "0F52BA", "E0115F", "AD6F69", "FF9408", "8F00FF", "A81C07", "D2691E", "F0DC82", "121212", "FFFFFF", "000000", "6A0DAD"),
  type = c("Iridescent", "Iridescent", "Iridescent", "Iridescent", 
           "Glittering", "Glittering", "Iridescent", "Pigmentary", 
           "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Iridescent"),
  is_natural_plumage = TRUE
)

# Merge all datasets and remove duplicates
final_dictionary <- bind_rows(tkg_sanitized_colors, core_14) %>%
  distinct(wikidata_qid, .keep_all = TRUE) %>%
  arrange(hbw_name)

# --- 5. DATA EXPORT ---
write.csv(final_dictionary, "data/color_dictionary_hbw_wikidata.csv", row.names = FALSE)

# Summary Output
cat("------------------------------------------------------------\n")
cat("TKG Semantic Dictionary Status:\n")
cat("Final count of sanitized color entities:", nrow(final_dictionary), "\n")
cat("Dictionary exported to: data/color_dictionary_hbw_wikidata.csv\n")
cat("------------------------------------------------------------\n")
