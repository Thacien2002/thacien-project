# ==============================================================================
# 🚀 RUN RWANDA NUTRITION DASHBOARD WITH REAL CSV DATA
# ==============================================================================
if (require("rstudioapi")) {
  # When running in RStudio, set the working directory to the script's directory
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # A fallback for running from the command line, assumes the script is run from the project root
  setwd(getwd())
}

# Check if app.r exists
if (!file.exists("app.r")) {
  stop("❌ app.r not found! Please ensure you're in the correct directory.")
}

cat("🔄 Loading Rwanda Nutrition Dashboard with real CSV data...\n")
cat("📁 Working directory:", getwd(), "\n")
cat("📊 Expected data file: R project/project/data/nutrition_data.csv\n\n")

# Source and run the app
source("app.r", encoding = "UTF-8")

# The app should now be running with your real CSV data!
cat("\n🎉 Dashboard launched successfully!\n")
cat("📈 Charts will display real data from your CSV file\n")
cat("🌍 Interactive maps and filters are ready\n")
cat("🇷🇼 Bilingual interface (Kinyarwanda & English) available\n")
cat("\n💡 Use the language switcher in the top-right corner\n")
cat("💡 Apply filters in the sidebar to explore your data\n")
cat("💡 All charts will update automatically based on your real data\n")
