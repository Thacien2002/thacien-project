# Test script for Trend Analysis and Data Explorer sections
# Set the working directory to the R project folder
project_path <- "C:/Users/USER/Desktop/thacien project/R project"
if (dir.exists(project_path)) {
  setwd(project_path)
  cat("Working directory set to:", getwd(), "\n")
} else {
  stop("Project path not found: ", project_path)
}

# Load the app
if (file.exists("app.r")) {
  source("app.r")
  cat("✅ app.r loaded successfully.\n")
  cat("🚀 Starting Shiny app...\n")
  cat("📊 Trend Analysis and Data Explorer sections should now load real data fast!\n")
  cat("🌐 The app will open in your browser shortly.\n")
  
  # Run the Shiny app
  shinyApp(ui, server)
} else {
  stop("app.r not found in the project directory.")
}
