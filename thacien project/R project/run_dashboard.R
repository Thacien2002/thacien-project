# Rwanda Nutrition Dashboard Launcher ----
# This script is designed to be run from the root of the R project.

# ---- Set Working Directory ----
# The 'here' package automatically finds the project root (the folder with the .Rproj file)
# and we will use it to build reliable paths.
project_root <- here::here()
cat("✅ Project root detected at:", project_root, "\n")

# Define the relative path to the R project from the detected root
r_project_path <- file.path("thacien project", "R project")

# ---- Install Dependencies ----
tryCatch({
  # Source the dependency installer from the project root
  source(here::here("thacien project", "R project", "install_dependencies.R"))
  cat("✅ Dependencies checked/installed.\n")
}, error = function(e) {
  stop("❌ FATAL ERROR: Could not run dependency installer. Please run 'install_dependencies.R' manually.")
})

# Load required libraries
library(shiny)
library(shinydashboard)
library(yaml)
library(here) # for portable paths
library(future) # for plan(multisession)

# ---- Pre-flight checks for essential files ----
required_files_relative <- c(
  "app.r", 
  "project/data/RWA_adm/RWA_adm2.shp"
)
files_exist <- sapply(required_files_relative, function(f) file.exists(here::here("thacien project", "R project", f)))

if (!all(files_exist)) {
  missing_files <- required_files_relative[!files_exist]
  stop("❌ FATAL ERROR: Cannot launch app. The following essential files are missing:\n- ",
       paste(missing_files, collapse = "\n- "))
}

cat("✅ All essential files found\n")

# ---- Source utility functions (with error handling) ----
tryCatch({
  # data_loader.R does not exist in the provided context. If it did, it would be sourced here.
  # source(here::here(r_project_path, "R", "data_loader.R")
}, error = function(e) {
  stop("❌ FATAL ERROR: Cannot load data. `R/data_loader.R` is missing or has an error.")
})

tryCatch({
  # data_utils.R does not exist. If it did, it would be sourced here.
  # source(here::here(r_project_path, "R", "data_utils.R")
}, error = function(e) {
  cat("⚠️ data_utils.R not found, skipping\n")
})

tryCatch({
  source(here::here("thacien project", "R project", "R", "gemini_utils.R"))
  cat("✅ gemini_utils.R loaded\n")
}, error = function(e) {
  stop("❌ FATAL ERROR: gemini_utils.R not found or has an error: ", e$message)
})

tryCatch({
  if (file.exists(here::here("thacien project", "R project", "R", "nisr_theme.R"))) {
    source(here::here("thacien project", "R project", "R", "nisr_theme.R"))
    cat("✅ nisr_theme.R loaded successfully\n")
  } else {
    cat("⚠️ R/nisr_theme.R not found, using default theme\n")
  }
}, error = function(e) {
  cat("⚠️ Failed to load NISR theme:", e$message, "\n")
  cat("📋 Using default theme instead\n")
})

# ---- Load configuration ----
config_path_relative <- file.path("thacien project", "R project", "config", "config.yml")
if (file.exists(here::here(config_path_relative))) {
  tryCatch({
    config <- config::get(file = here::here("thacien project", "R project", "config", "config.yml"))
    cat("✅ config.yml loaded\n")
  }, error = function(e) {
    cat("⚠️ config.yml found but failed to load, using defaults\n")
  })
} else {
  cat("⚠️ config.yml not found, using defaults\n")
}

# ---- Enable background processing ----
if (requireNamespace("future", quietly = TRUE)) {
  tryCatch({
    plan(multisession)
    cat("✅ Background processing enabled\n")
  }, error = function(e) {
    cat("⚠️ Background processing failed, continuing without it\n")
  })
}

# ---- Set config file path globally for the app ----
full_config_path <- here::here(config_path_relative)
if (file.exists(full_config_path)) {
  options(config.file = full_config_path)
  cat("✅ Config file path set globally for the app session:", full_config_path, "\n")
} else {
  cat("⚠️ Could not find config.yml to set globally. The app may use default values.\n")
}

# ---- Run the Shiny app ----
cat("🚀 Launching Shiny app...\n")
runApp(here::here("thacien project", "R project", "app.r"), launch.browser = TRUE)
