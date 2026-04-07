# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# FIGURE 3: Full Biogeographic Hybrid Map (Final Version)
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

# --- 2. DATA LOADING & SPATIAL CLEANING ---
input_path <- "data/tkg_hummingbirds_research_grade.csv"
if(!file.exists(input_path)) stop("Input file missing in /data folder.")

raw_data <- read.csv(input_path, stringsAsFactors = FALSE)
clean_data <- raw_data %>% filter(!is.na(longitude) & !is.na(latitude))

# Spatial processing
inat_sf_raw <- st_as_sf(clean_data, coords = c("longitude", "latitude"), crs = 4326)
world_ref <- ne_countries(scale = 50, returnclass = "sf") %>% 
  st_make_valid() %>% 
  select(continent, name)

# Spatial join to filter only Americas
points_joined <- st_join(inat_sf_raw, world_ref, join = st_intersects)
clean_data_americas <- points_joined %>%
  filter(continent %in% c("North America", "South America")) %>%
  filter(!is.na(name))

# --- 3. THE FIX: RE-EXTRACT COORDINATES ---
# stat_density_2d needs explicit X and Y columns, but st_join hides them in 'geometry'
coords <- st_coordinates(clean_data_americas)
clean_data_final <- clean_data_americas %>%
  mutate(longitude = coords[,1],
         latitude = coords[,2])

# --- 4. TERRAIN RELIEF ---
bbox_americas <- data.frame(x = c(-170, -30), y = c(-60, 75))
message("[SYSTEM] Fetching Elevation Relief (DEM)...")
elev_raster <- get_elev_raster(bbox_americas, prj = "EPSG:4326", z = 2, clip = "bbox")
elev_terra <- rast(elev_raster)

# --- 5. VISUALIZATION ---
tkg_map_final <- ggplot() +
  geom_spatraster(data = elev_terra) +
  scale_fill_gradientn(colors = c("#21618C", "#2E86C1", "#AED6F1", "#F7DC6F", "#E67E22", "#A04000", "#FBFCFC"),
                       values = scales::rescale(c(-5000, 0, 1, 1000, 2500, 4500, 8000)),
                       guide = "none") +
  new_scale_fill() +
  
  geom_sf(data = world_ref, fill = NA, color = "white", size = 0.1, alpha = 0.2) +
  
  # Now 'longitude' and 'latitude' exist again for this layer
  stat_density_2d(data = clean_data_final, aes(x = longitude, y = latitude, fill = after_stat(level)), 
                  geom = "polygon", alpha = 0.4, bins = 15) +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  
  geom_sf(data = clean_data_final, color = "white", size = 0.4, alpha = 0.7) +
  geom_sf(data = clean_data_final, color = "#0E6251", size = 0.1, alpha = 0.5) +
  
  coord_sf(crs = 4326, xlim = c(-170, -30), ylim = c(-60, 75)) +
  
  labs(title = "Biogeographic Distribution of Trochilidae Phenotypes",
       subtitle = paste("Hybrid mapping of S =", n_distinct(clean_data_final$scientific_name), "species"),
       caption = "TKG Pipeline 2026 | Elevation: AWS DEM | Projection: WGS84") +
  
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.background = element_rect(fill = "#EBF5FB", color = NA),
        panel.grid = element_blank())

# --- 6. EXPORT ---
dir.create("results", showWarnings = FALSE)
ggsave("results/Fig3_TKG_Hybrid_Distribution_FINAL.png", plot = tkg_map_final, width = 9, height = 12, dpi = 600)
ggsave("results/Fig3_TKG_Hybrid_Distribution_FINAL.pdf", plot = tkg_map_final, width = 9, height = 12)
cat("[SUCCESS] Figure generated.\n")
