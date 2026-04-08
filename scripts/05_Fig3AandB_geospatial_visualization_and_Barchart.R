# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# FIGURE 3: Biogeographic Hybrid Map & Taxonomic Audit (INTEGRATED)
# AUTHOR: IsraelMV (UNAM)
# ==============================================================================

# --- 1. DEPENDENCY MANAGEMENT ---
required_packages <- c("tidyverse", "sf", "elevatr", "terra", "tidyterra", 
                       "viridis", "ggnewscale", "rnaturalearth", "rnaturalearthdata")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(tidyverse)
library(sf)
library(elevatr)
library(terra)
library(tidyterra)
library(viridis)
library(ggnewscale)
library(rnaturalearth)

# --- 2. DATA LOADING (INTEGRATING MASSIVE THINNED DATA) ---
# We now point to the output of module 01b to ensure bias-free density
input_path <- "data/processed/massive_thinned_observations.csv"
if(!file.exists(input_path)) stop("Massive thinned data missing. Run script 01b first.")

# Loading thinned data while preserving taxonomic identity
massive_data <- read.csv(input_path, stringsAsFactors = FALSE)

# Convert to spatial object for Join
inat_sf <- st_as_sf(massive_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Reference layers
world_ref <- ne_countries(scale = 50, returnclass = "sf") %>% 
  st_make_valid() %>% 
  select(continent, name)

# Spatial join to ensure we only map the Americas and keep country metadata
points_joined <- st_join(inat_sf, world_ref, join = st_intersects)
clean_data_final <- points_joined %>%
  filter(continent %in% c("North America", "South America")) %>%
  filter(!is.na(name))

# --- 3. TERRAIN RELIEF (DEM) ---
bbox_americas <- data.frame(x = c(-170, -30), y = c(-60, 75))
message("[SYSTEM] Fetching Elevation Relief (DEM)...")
elev_raster <- get_elev_raster(bbox_americas, prj = "EPSG:4326", z = 2, clip = "bbox")
elev_terra <- rast(elev_raster)

# --- 4. FIGURE 3A: HYBRID MAP ---
tkg_map_final <- ggplot() +
  # Terrain Layer
  geom_spatraster(data = elev_terra) +
  scale_fill_gradientn(colors = c("#21618C", "#2E86C1", "#AED6F1", "#F7DC6F", "#E67E22", "#A04000", "#FBFCFC"),
                       values = scales::rescale(c(-5000, 0, 1, 1000, 2500, 4500, 8000)),
                       guide = "none") +
  new_scale_fill() +
  
  # Country Borders
  geom_sf(data = world_ref, fill = NA, color = "white", size = 0.1, alpha = 0.2) +
  
  # Density Layer (Now based on thinned biological data)
  stat_density_2d(data = clean_data_final, aes(x = longitude, y = latitude, fill = after_stat(level)), 
                  geom = "polygon", alpha = 0.4, bins = 15) +
  scale_fill_viridis_c(option = "magma", name = "Biological\nDensity",
                       breaks = c(0.001, 0.005), labels = c("Low", "High")) +
  
  # Individual Thinned Points (Preserving identity)
  geom_sf(data = clean_data_final, color = "white", size = 0.2, alpha = 0.5) +
  
  coord_sf(crs = 4326, xlim = c(-120, -30), ylim = c(-55, 55)) + # Focused on core distribution
  labs(title = "Biogeographic Distribution of Trochilidae Phenotypes",
       subtitle = paste("Massive mapping of S =", n_distinct(clean_data_final$scientific_name), "taxa"),
       caption = "TKG Pipeline 2026 | Spatial Thinning: 10km2 | Projection: WGS84") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.background = element_rect(fill = "#EBF5FB", color = NA),
        panel.grid = element_blank())

# --- 5. FIGURE 3B: TAXONOMIC RICHNESS (INTEGRITY CHECK) ---
# We focus on the Americas and select the top 15 countries by species richness
audit_data <- clean_data_final %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise(
    n_species = n_distinct(scientific_name), 
    n_observations = n()
  ) %>%
  arrange(desc(n_species)) %>%
  slice_head(n = 15)

fig_3b_bar_chart <- ggplot(audit_data, aes(x = reorder(name, n_species), y = n_species, fill = n_species)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  
  # Adding the exact number with a slight offset and ensuring it stays within bounds
  geom_text(aes(label = n_species), 
            hjust = -0.3, 
            family = "serif", 
            size = 3.5, 
            fontface = "bold",
            color = "#17202A") +
  
  # Scientific color palette
  scale_fill_viridis_c(option = "magma", begin = 0.3, end = 0.8) +
  
  # THE FIX: Expand the y-axis (which is the x-axis after flip) and disable clipping
  coord_flip(clip = "off") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  
  labs(
    title = "Taxonomic Richness by Country",
    subtitle = "Top 15 countries by validated species richness (S)",
    x = NULL, 
    y = "Number of Species (S)"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, face = "italic"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10) # Added right margin buffer
  )

# --- 6. EXPORT ---
ggsave("results/Fig3A_TKG_Hybrid_Map_MASSIVE.png", plot = tkg_map_final, width = 9, height = 12, dpi = 600)
ggsave("results/Fig3B_TKG_Richness_MASSIVE.png", plot = fig_3b_bar_chart, width = 7, height = 6, dpi = 600)

