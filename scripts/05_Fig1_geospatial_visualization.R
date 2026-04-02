# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# FIGURE 3: Full Continental Distribution
# ==============================================================================

# 1. CARGA Y LIMPIEZA AUTOMÁTICA
raw_data <- read.csv("data/tkg_hummingbirds_research_grade.csv")

# Filtro de integridad: Solo registros con Coordenadas
clean_data <- raw_data %>%
  filter(!is.na(longitude) & !is.na(latitude))

# Cálculo de la N final para el paper
n_final <- nrow(clean_data)
n_perdidos <- nrow(raw_data) - n_final

cat("Resumen de Integridad:\n")
cat("- Registros totales: ", nrow(raw_data), "\n")
cat("- Registros con NA (eliminados): ", n_perdidos, "\n")
cat("- N final para el mapa y TKG: ", n_final, "\n")

# Convertir a Objeto Espacial
inat_sf <- st_as_sf(clean_data, coords = c("longitude", "latitude"), crs = 4326)

# 2. MAPA BASE Y PROYECCIÓN (SIN CORTES)
ne_countries <- ne_countries(scale = 50, continent = c("north america", "south america"), returnclass = "sf")

# 3. VISUALIZACIÓN
tkg_map_full <- ggplot() +
  geom_sf(data = st_graticule(crs = 4326), color = "#D5D8DC", size = 0.1, linetype = "dotted") +
  geom_sf(data = ne_countries, fill = "#F8F9F9", color = "#B2BABB", size = 0.2) +
  geom_sf(data = inat_sf, color = "#D4AC0D", size = 0.5, alpha = 0.3) +
  
  # Ajuste de límites para Canadá y Brasil
  coord_sf(crs = "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
           xlim = c(-8000000, 6000000), ylim = c(-6500000, 8500000)) +
  
  # Título dinámico: Usa paste() para que el número cambie solo
  labs(title = "Figure 1 | Continental Scale of Trochilidae Phenotypic Evidence",
       subtitle = paste("Geospatial distribution of n =", n_final, "validated observations"),
       caption = paste("Excluded", n_perdidos, "records without coordinates | TKG Pipeline 2026"),
       x = "Longitude", y = "Latitude") +
  
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, color = "#17202A"),
    plot.subtitle = element_text(size = 11, color = "#566573"),
    panel.background = element_rect(fill = "#FBFCFC", color = NA),
    axis.text = element_text(size = 7, color = "#AEB6BF"),
    axis.title = element_text(size = 9, color = "#85929E")
  )

# 4. GUARDAR
ggsave("results/Fig1_Full_Continental_Map_Final.png", plot = tkg_map_full, width = 8, height = 11, dpi = 600)
ggsave("results/Fig1_Full_Continental_Map_Final.pdf", plot = tkg_map_full, width = 8, height = 11)
