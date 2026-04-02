# ==============================================================================
# PROJECT: Trochilidae Knowledge Graph (TKG)
# STEP 04: Smart Interactive Curator (Full Species-Wide & Semantic Search)
# ==============================================================================
# AUTHOR: IsraelMV (UNAM)
# FILE: scripts/04_tkg_interactive_curator.R
# DESCRIPTION: 
# Professional annotation loop. Aggregates all unique species evidence, 
# manages multi-photo displays, and uses a keyword-based semantic engine
# to suggest standardized Wikidata color entities from the HBW dictionary.
# ==============================================================================


if (!require("rstudioapi")) install.packages("rstudioapi")
library(dplyr)
library(stringr)
library(stringdist)

# --- 1. FILE INTELLIGENCE ---
session_files <- list.files("data/", pattern = "^TKG_Annotation_.*\\.csv$", full.names = TRUE)

if(length(session_files) > 0) {
  input_file <- session_files[order(file.info(session_files)$mtime, decreasing = TRUE)[1]]
  message("[SYSTEM] Resuming from: ", input_file)
} else {
  input_file <- "data/TKG_Evidence_Annotation_N-Obs.csv"
  message("[SYSTEM] Starting from master matrix.")
}

color_db <- read.csv("data/color_dictionary_hbw_wikidata.csv", stringsAsFactors = FALSE)
annotation_matrix <- read.csv(input_file, stringsAsFactors = FALSE)

# --- 2. GUI LOGIN ---
current_annotator <- rstudioapi::showPrompt("Login", "Who is performing the curation?", "Israel")
if (is.null(current_annotator) || current_annotator == "") current_annotator <- "Unknown_User"

session_time <- format(Sys.time(), "%Y%m%d_%H%M")
output_file <- paste0("data/TKG_Annotation_", current_annotator, "_", session_time, ".csv")

# --- 3. HELPER FUNCTIONS ---

get_color_gui <- function(region_name, db) {
  while(TRUE) {
    msg <- paste0("Color for ", toupper(region_name), " (or 'DD'):")
    user_input <- rstudioapi::showPrompt("Color Entry", msg, "")
    
    if(is.null(user_input)) return(NULL) 
    user_input <- str_trim(user_input)
    
    if(toupper(user_input) == "DD") return(list(hbw_name="Data Deficient", wikidata_qid="Q106512361", type="NA"))
    if(tolower(user_input) %in% c("withe", "whit")) user_input <- "white"
    
    match <- db %>% filter(tolower(hbw_name) == tolower(user_input))
    if(nrow(match) > 0) return(as.list(match[1,]))
    
    # Suggestions logic
    suggestions <- db %>% filter(str_detect(tolower(hbw_name), tolower(user_input))) %>% pull(hbw_name)
    if(length(suggestions) == 0) {
      dist_vector <- stringdist(tolower(user_input), tolower(db$hbw_name))
      suggestions <- db$hbw_name[order(dist_vector)[1:12]]
    }
    
    selected <- select.list(suggestions, title = paste("Select color for", region_name), graphics = TRUE)
    if(selected != "") return(as.list(db[db$hbw_name == selected, ][1,]))
  }
}

# --- 4. MAIN LOOP ---
pending_species <- annotation_matrix %>% 
  filter(is.na(wikidata_color_Q) | wikidata_color_Q == "" | wikidata_color_Q == "NA") %>% 
  distinct(scientific_name)

if(nrow(pending_species) == 0) {
  rstudioapi::showDialog("Done", "All species are already curated!")
} else {
  for(i in 1:nrow(pending_species)) {
    curr_sp <- pending_species$scientific_name[i]
    
    # 4.1 Multi-Photo Evidence
    all_sp_images <- annotation_matrix %>% filter(scientific_name == curr_sp) %>% pull(image_url) %>% unique()
    num_imgs <- length(all_sp_images)
    
    n_to_open <- rstudioapi::showPrompt("Evidence", 
                                        paste0(curr_sp, ": ", num_imgs, " photos found. How many to open?"), "3")
    n_to_open <- as.integer(n_to_open)
    
    if(!is.na(n_to_open) && n_to_open > 0) {
      for(img in head(all_sp_images, n_to_open)) browseURL(img)
    }
    
    # 4.2 Sex Identification
    sex_val <- select.list(c("Male", "Female", "Juvenile", "Unknown"), 
                           title = paste("Sex for:", curr_sp), graphics = TRUE)
    if(sex_val == "") sex_val <- "Unknown"
    
    # 4.3 Anatomical Regions
    species_answers <- list()
    regions <- c("Back", "Belly", "Bill", "Breast", "Chin", "Crest", "Crown", 
                 "Flanks", "Gorget", "Nape", "Rectrices", "Remiges", "Rump", "Tail coverts")
    
    for(reg in regions) {
      res <- get_color_gui(reg, color_db)
      if(is.null(res)) break
      species_answers[[reg]] <- res
    }
    
    # 4.4 Propagation & Save
    for(reg in regions) {
      rows <- annotation_matrix$scientific_name == curr_sp & annotation_matrix$region == reg
      annotation_matrix[rows, "wikidata_color_Q"] <- species_answers[[reg]]$wikidata_qid
      annotation_matrix[rows, "optical_property"] <- species_answers[[reg]]$type
      annotation_matrix[rows, "sex_P21"] <- sex_val
      annotation_matrix[rows, "annotator_metadata"] <- paste0("User: ", current_annotator, " | Session: ", session_time)
    }
    
    write.csv(annotation_matrix, output_file, row.names = FALSE)
    
    # 4.5 Continue?
    cont <- rstudioapi::showQuestion("Progress Saved", 
                                     paste0(curr_sp, " is done. Continue to next species?"), "Yes", "No")
    if(!cont) break
  }
}
