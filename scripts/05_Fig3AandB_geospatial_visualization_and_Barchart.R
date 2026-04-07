# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# FIGURE 3A: Full Biogeographic Hybrid Map (Final Version)
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
  # Terrain Layer
  geom_spatraster(data = elev_terra) +
  scale_fill_gradientn(colors = c("#21618C", "#2E86C1", "#AED6F1", "#F7DC6F", "#E67E22", "#A04000", "#FBFCFC"),
                       values = scales::rescale(c(-5000, 0, 1, 1000, 2500, 4500, 8000)),
                       guide = "none") +
  new_scale_fill() +
  
  # Country Borders
  geom_sf(data = world_ref, fill = NA, color = "white", size = 0.1, alpha = 0.2) +
  
  # Density Layer (Modified Legend here)
  stat_density_2d(data = clean_data_final, aes(x = longitude, y = latitude, fill = after_stat(level)), 
                  geom = "polygon", alpha = 0.4, bins = 15) +
  scale_fill_viridis_c(
    option = "magma", 
    name = "Relative\nDensity",
    breaks = c(0.001, 0.005), 
    labels = c("Low", "High")
  ) +
  
  # Individual Points
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

cat("[SUCCESS] Figure generated with professional relative density scale.\n")



# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# FIGURE 3B: Taxonomic Richness by Country (Spatial Audit)
# AUTHOR: IsraelMV (UNAM)
# ==============================================================================

# --- 1. AUTOMATED DEPENDENCY MANAGEMENT ---
required_packages <- c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "viridis")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)

# --- 2. DATA LOADING & SPATIAL JOIN ---
input_path <- "data/tkg_hummingbirds_research_grade.csv"
if(!file.exists(input_path)) stop("Input file missing.")

raw_data <- read.csv(input_path, stringsAsFactors = FALSE)
clean_data <- raw_data %>% filter(!is.na(longitude) & !is.na(latitude))

# Convert to spatial object
inat_sf <- st_as_sf(clean_data, coords = c("longitude", "latitude"), crs = 4326)

# Load and clean world boundaries
message("[SYSTEM] Loading geographic reference layers...")
world <- ne_countries(scale = 50, returnclass = "sf") %>%
  select(name, continent) %>%
  st_make_valid()

# Perform Spatial Join (Point-in-Polygon)
points_with_country <- st_join(inat_sf, world, join = st_intersects)

# --- 3. DATA AGGREGATION & FILTERING ---
# We focus on the Americas and select the top 15 countries by species richness
audit_data <- points_with_country %>%
  st_drop_geometry() %>%
  filter(continent %in% c("North America", "South America")) %>%
  group_by(name) %>%
  summarise(
    n_species = n_distinct(scientific_name),
    n_observations = n()
  ) %>%
  arrange(desc(n_species)) %>%
  slice_head(n = 15) # Top 15 countries

# --- 4. VISUALIZATION (Publication Quality) ---
fig_3b_bar_chart <- ggplot(audit_data, aes(x = reorder(name, n_species), y = n_species, fill = n_species)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  # Adding the exact number at the end of each bar for clarity
  geom_text(aes(label = n_species), hjust = -0.3, family = "serif", size = 3.5, color = "#17202A") +
  
  # Scientific color palette (matching the map's aesthetic)
  scale_fill_viridis_c(option = "magma", begin = 0.3, end = 0.8) +
  
  coord_flip() + # Flip for better readability of country names
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Extra space for labels
  
  labs(
    title = "Taxonomic Representation by Country",
    subtitle = "Top 15 countries in the TKG by species richness (S)",
    x = NULL,
    y = "Number of Species (S)"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, face = "italic"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- 5. EXPORT ---
dir.create("results", showWarnings = FALSE)
ggsave("results/Fig3B_TKG_Taxonomic_Richness.png", plot = fig_3b_bar_chart, width = 7, height = 6, dpi = 600)
ggsave("results/Fig3B_TKG_Taxonomic_Richness.pdf", plot = fig_3b_bar_chart, width = 7, height = 6)

cat("[SUCCESS] Figure 3B generated. Check /results/ folder.\n")
