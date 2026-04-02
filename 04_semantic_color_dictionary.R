# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 04: Semantic Color Dictionary (HBW to Wikidata Mapping)
# ==============================================================================

color_mapping <- data.frame(
  hbw_name = c("Emerald Green", "Golden Green", "Sapphire Blue", "Ruby Red", 
               "Glittering Copper", "Fiery Orange", "Violet", "Rufous", 
               "Cinnamon", "Buff", "Sooty", "White", "Black", "Iridescent Purple"),
  wikidata_qid = c("Q691510", "Q5579480", "Q108183", "Q1120984", 
                   "Q5168705", "Q1378330", "Q8063", "Q7377981", 
                   "Q1148100", "Q4985885", "Q7562417", "Q23444", "Q23445", "Q211140"),
  type = c("Iridescent", "Iridescent", "Iridescent", "Iridescent", 
           "Glittering", "Glittering", "Iridescent", "Pigmentary", 
           "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Pigmentary", "Iridescent"),
  stringsAsFactors = FALSE
)

# Guardar en la carpeta data para que el script de triplificación lo encuentre
write.csv(color_mapping, "data/color_dictionary_hbw_wikidata.csv", row.names = FALSE)

cat("Diccionario Semántico creado: data/color_dictionary_hbw_wikidata.csv\n")