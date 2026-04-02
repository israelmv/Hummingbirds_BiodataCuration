# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 04: Biocentric Phenotypic Lexicon Generation (Data Sanitization)
# ==============================================================================
# AUTHOR: IsraelMV (UNAM)
# DESCRIPTION: 
# This script executes a multi-stage filtration protocol to convert raw Wikidata 
# color entities into a specialized dictionary for hummingbird plumage annotation. 
#
# FILTRATION RATIONALE:
# 1. Removal of cellular/metabolic processes (e.g., 'pigment cell development').
# 2. Exclusion of commercial/corporate branding (e.g., 'Android Green', 'UCLA Blue').
# 3. Elimination of industrial color standards (Pantone, RAL, ISCC).
# 4. Suppression of 'Ghost Entities' (Entries lacking natural language labels).
# 5. Requirement for Hexadecimal triplets to ensure computational colorimetry.
# ==============================================================================

library(dplyr)
library(stringr)

# --- 1. DATA ACQUISITION ---
# Load the raw results from the Wikidata Query Service (WDQS)
raw_wikidata <- read.csv("data/query.csv", stringsAsFactors = FALSE)

# --- 2. BIOCENTRIC FILTRATION PROTOCOL (ULTRA-STRICT) ---
tkg_sanitized_colors <- raw_wikidata %>%
  
  # Protocol A: Remove non-phenotypic biological/metabolic noise
  filter(!str_detect(colorLabel, "(?i)process|cell|differentiation|granule|metabolic|developmental|hair")) %>%
  
  # Protocol B: Remove commercial, political, and institutional brand colors
  filter(!str_detect(colorLabel, "(?i)Microsoft|Android|Discord|UCLA|Yale|Pitufo|national|Ukraine|Google|Apple|Facebook")) %>%
  
  # Protocol C: INDUSTRIAL CLEANING (Strict exclusion of industrial color standards)
  # This preserves the natural nomenclatural integrity required for HBW compliance.
  filter(!str_detect(colorLabel, "(?i)Pantone|RAL [0-9]|RAL color|ISCC")) %>%
  
  # Protocol D: REMOVE ANONYMOUS ENTITIES (Labels that consist solely of QIDs)
  # Removes entries such as 'Q1499005' where no descriptive label is available.
  filter(!str_detect(colorLabel, "^Q[0-9]+$")) %>%
  
  # Protocol E: Visual Data Integrity (Requirement for Hexadecimal validation)
  # Preserves critical avian descriptors even if Hex metadata is absent.
  filter(nchar(hex) > 0 | colorLabel %in% c("Sooty", "brindle")) %>%
  
  # Protocol F: QID Extraction from URI
  mutate(wikidata_qid = str_extract(color, "Q[0-9]+"))

# --- 3. PHENOTYPIC CLASSIFICATION ---
# Categorize colors into Iridescent (Structural) vs. Pigmentary properties.
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

# --- 4. HBW/ARIZMENDI BASELINE INTEGRATION (CORE 14) ---
# Hard-coded baseline ensuring compatibility with the 14-region segmentation model.
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

# Merge datasets and resolve taxonomic/chromatic redundancies
final_dictionary <- bind_rows(tkg_sanitized_colors, core_14) %>%
  distinct(wikidata_qid, .keep_all = TRUE) %>%
  arrange(hbw_name)

# --- 5. EXPORT ---
# Outputting the master dictionary to the repository's data directory.
write.csv(final_dictionary, "data/color_dictionary_hbw_wikidata.csv", row.names = FALSE)

# Execution Summary
cat("------------------------------------------------------------\n")
cat("TKG SEMANTIC DICTIONARY PIPELINE COMPLETED\n")
cat("Final sanitized color entity count: ", nrow(final_dictionary), "\n")
cat("Output saved to: data/color_dictionary_hbw_wikidata.csv\n")
cat("------------------------------------------------------------\n")
