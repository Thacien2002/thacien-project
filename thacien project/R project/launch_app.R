# ==============================================================================
# 🚀 SIMPLE APP LAUNCHER (using here::here for safe paths)
# ==============================================================================

cat("🚀 Launching Burundi Nutrition Dashboard...\n")
cat("==========================================\n\n")

# --------------------------------------------------------------------------
# 📚 Load required libraries
# --------------------------------------------------------------------------
library(here)   # for safe paths
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)
library(leaflet)
library(readr)
library(DT)
library(glue)

# --------------------------------------------------------------------------
# 📂 Ensure project root is set
# --------------------------------------------------------------------------
# IMPORTANT: this line tells {here} where the project root is.
# Place this file (launch_app.R) in your main project folder.
here::i_am("launch_app.R")

# --------------------------------------------------------------------------
# 📊 Load nutrition data
# --------------------------------------------------------------------------
csv_path <- here("project", "data", "nutrition_data.csv")

if (file.exists(csv_path)) {
  full_data <- read.csv(csv_path)
  cat("✅ Nutrition data CSV loaded successfully from:", csv_path, "\n")
} else {
  cat("❌ Nutrition data CSV NOT FOUND\n")
  cat("📁 Expected location:", csv_path, "\n")
  stop("App cannot run without nutrition_data.csv")
}

# --------------------------------------------------------------------------
# 🎨 Load theme
# --------------------------------------------------------------------------
cat("🎨 Loading theme...\n")
theme_path <- here("R", "nisr_theme.R")

if (file.exists(theme_path)) {
  tryCatch({
    library(fresh)
    library(bslib)
    source(theme_path)
    cat("✅ Theme loaded successfully\n")
  }, error = function(e) {
    cat("⚠️ Theme loading failed:", e$message, "\n")
    cat("Continuing without custom theme...\n")
  })
} else {
  cat("⚠️ Theme file not found, using default theme\n")
}

# --------------------------------------------------------------------------
# 🔧 Load utility files
# --------------------------------------------------------------------------
cat("🔧 Loading utility functions...\n")

data_utils_path <- here("R", "data_utils.R")
if (file.exists(data_utils_path)) {
  source(data_utils_path)
  cat("✅ Data utilities loaded\n")
}

gemini_utils_path <- here("R", "gemini_utils.R")
if (file.exists(gemini_utils_path)) {
  source(gemini_utils_path)
  cat("✅ Gemini utilities loaded\n")
}

# --------------------------------------------------------------------------
# 🚀 Launch the Shiny app
# --------------------------------------------------------------------------
cat("\n🚀 Launching Shiny app...\n")
cat("==========================================\n")

app_path <- here("app.r")
if (file.exists(app_path)) {
  runApp(app_path, display.mode = "normal")
} else {
  cat("❌ app.R file not found at:", app_path, "\n")
}
