# Simple launcher - works from any directory
cat("🚀 Simple Rwanda Nutrition Dashboard Launcher\n")
cat("============================================\n\n")

# Set the correct working directory
project_path <- "C:/Users/USER/Desktop/thacien project/R project"
setwd(project_path)

cat("📁 Working directory set to:", getwd(), "\n")

# Check if app.r exists
if (!file.exists("app.r")) {
  cat("❌ app.r not found in:", getwd(), "\n")
  cat("📋 Available files:\n")
  cat(paste(list.files(), collapse = "\n"), "\n")
  stop("Cannot find app.r")
}

cat("✅ Found app.r\n")

# Load required libraries
cat("📚 Loading libraries...\n")
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(readr)

# Try to load theme (optional)
if (file.exists("R/nisr_theme.R")) {
  tryCatch({
    library(fresh)
    library(bslib)
    source("R/nisr_theme.R")
    cat("✅ NISR theme loaded successfully\n")
  }, error = function(e) {
    cat("⚠️ Theme failed to load:", e$message, "\n")
    cat("📋 Using default theme\n")
  })
} else {
  cat("⚠️ R/nisr_theme.R not found, using default theme\n")
}

# Launch the app
cat("🚀 Launching app...\n")
runApp("app.r")
