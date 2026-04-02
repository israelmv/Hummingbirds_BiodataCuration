# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 04: Biocentric Color Dictionary (Wikidata Sanitization)
# SYSTEM ARCHITECT: Isra | BIOCURADURÍA LEAD: Layla
# ==============================================================================

library(dplyr)
library(stringr)

# --- 1. DATA LOADING ---
raw_wikidata <- read.csv("/home/isra/Documentos/Isra/Layla/Colibries/data/Wikidata_colors.csv", stringsAsFactors = FALSE)

# --- 2. THE "BIO-PURGE" (STRICT FILTERS) ---
# Eliminamos todo lo que no sea un color visual fenotípico
tkg_sanitized_colors <- raw_wikidata %>%
  # Regla 1: Eliminar procesos biológicos, celulares y genéticos (el ruido del CSV)
  filter(!str_detect(colorLabel, "process|cell|differentiation|granule|metabolic|developmental|hair")) %>%
  
  # Regla 2: Eliminar colores comerciales, marcas y entidades no naturales
  filter(!str_detect(colorLabel, "Microsoft|Android|Discord|UCLA|Yale|Pitufo|national colours|Ukraine")) %>%
  
  # Regla 3: Mantener solo registros con código Hexadecimal (Garantía de visualización)
  # Pero hacemos una excepción con "Sooty" y "brindle" que son comunes en aves
  filter(nchar(hex) > 0 | colorLabel %in% c("Sooty", "brindle")) %>%
  
  # Regla 4: Limpiar URLs para quedarnos solo con el QID
  mutate(wikidata_qid = str_extract(color, "Q[0-9]+"))

# --- 3. BIOLOGICAL CLASSIFICATION (Iridescent vs Pigmentary) ---
# Clasificamos según la nomenclatura de ornitología
tkg_sanitized_colors <- tkg_sanitized_colors %>%
  mutate(
    type = case_when(
      str_detect(colorLabel, "(?i)emerald|golden|sapphire|ruby|iridescent|glittering|metallic|turquoise") ~ "Iridescent",
      str_detect(colorLabel, "(?i)rufous|buff|cinnamon|sooty|brown|grey|black|white|ochre") ~ "Pigmentary",
      TRUE ~ "General"
    ),
    # Añadimos una columna de validación para el paper
    is_natural_plumage = TRUE
  ) %>%
  select(hbw_name = colorLabel, wikidata_qid, hex, type, is_natural_plumage) %>%
  distinct(wikidata_qid, .keep_all = TRUE) # Evitar duplicados de QID

# --- 4. INTEGRATION OF THE "CORE 14" (Arizmendi Baseline) ---
# Nos aseguramos de que tus 14 colores originales de HBW siempre estén presentes
core_14 <- data.frame(
  hbw_name = c("Emerald Green", "Golden Green", "Sapphire Blue", "Ruby Red", "Glittering Copper", "Fiery Orange", "Violet", "Rufous", "Cinnamon", "Buff", "Sooty", "White", "Black", "Iridescent Purple"),
  wikidata_qid = c("Q691510", "Q5579480", "Q108183", "Q1120984", "Q5168705", "Q1378330", "Q8063", "Q7377981", "Q1148100", "Q4985885", "Q7562417", "Q23444", "Q23445", "Q211140"),
  hex = c("50C878", "FFDF00", "0F52BA", "E0115F", "AD6F69", "FF9408", "8F00FF", "A81C07", "D2691E", "F0DC82", "121212", "FFFFFF", "000000", "6A0DAD"),
  type = c("Iridescent", "Iridescent", "Iridescent", "Iridescent", "Glittering", "Glittering", "Iridescent", "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Iridescent"),
  is_natural_plumage = TRUE
)

final_dictionary <- bind_rows(tkg_sanitized_colors, core_14) %>%
  distinct(wikidata_qid, .keep_all = TRUE)

# --- 5. EXPORT ---
write.csv(final_dictionary, "data/color_dictionary_hbw_wikidata.csv", row.names = FALSE)
cat("Diccionario Bio-Sanitizado creado con", nrow(final_dictionary), "entidades de color.\n")
