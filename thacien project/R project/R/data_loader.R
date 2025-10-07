# ==============================================================================
# 🔹 DATA LOADER
# This script centralizes data loading for the Shiny application.
# ==============================================================================

# Define file paths
NUTRITION_DATA_PATH <- "project/data/nutrition_data.csv"
SHAPEFILE_PATH <- "project/data/RWA_adm/RWA_adm2.shp"

#' Load Rwanda Nutrition Data from CSV
#'
#' Reads the nutrition data, specifies column types, and adds a 'country' column.
#' @return A data frame with the nutrition data, or an empty data frame on error.
load_rwanda_nutrition_data <- function() {
  if (!file.exists(NUTRITION_DATA_PATH)) {
    cat("❌ CSV file not found:", NUTRITION_DATA_PATH, "\n")
    return(data.frame())
  }
  
  tryCatch({
    df <- readr::read_csv(NUTRITION_DATA_PATH, 
                         col_types = readr::cols(
                           district = readr::col_character(),
                           province = readr::col_character(),
                           year = readr::col_integer(),
                           age_group = readr::col_character(),
                           nutrient = readr::col_character(),
                           stunting = readr::col_double(),
                           wasting = readr::col_double(),
                           anemia = readr::col_double(),
                           poverty_rate = readr::col_double(),
                           maternal_education = readr::col_double(),
                           sanitation_access = readr::col_double(),
                           food_diversity = readr::col_double(),
                           health_access_km = readr::col_double()
                         ))
    
    # Add country column to ensure only Rwanda data is used
    df$country <- "Rwanda"
    
    cat("✅ Nutrition data loaded successfully from:", NUTRITION_DATA_PATH, "\n")
    return(df)
    
  }, error = function(e) {
    cat("❌ Error reading CSV file:", e$message, "\n")
    return(data.frame())
  })
}

#' Load Rwanda Districts Shapefile
#'
#' Reads the shapefile for Rwanda's administrative boundaries.
#' @return An sf object with the shapefile data, or NULL on error.
load_rwanda_shapefile <- function() {
  if (!file.exists(SHAPEFILE_PATH)) {
    cat("❌ Shapefile not found:", SHAPEFILE_PATH, "\n")
    return(NULL)
  }
  
  tryCatch({
    sf_data <- sf::st_read(SHAPEFILE_PATH, quiet = TRUE)
    cat("✅ Shapefile loaded successfully from:", SHAPEFILE_PATH, "\n")
    return(sf_data)
  }, error = function(e) {
    cat("❌ Error loading shapefile:", e$message, "\n")
    return(NULL)
  })
}

# Load data into global environment
full_data <- load_rwanda_nutrition_data()
rwanda_districts_sf <- load_rwanda_shapefile()