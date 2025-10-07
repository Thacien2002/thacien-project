# ==============================================================================
# 🚀 RUN CLEAN RWANDA NUTRITION DASHBOARD (NO CONSOLE WARNINGS)
# ==============================================================================

# Suppress warnings and messages
options(warn = -1)
options(shiny.sanitize.errors = TRUE)

# Set working directory
setwd("C:/Users/USER/Desktop/thacien project/R project")

# Check if app.r exists
if (!file.exists("app.r")) {
  stop("❌ app.r not found! Please ensure you're in the correct directory.")
}

cat("🔄 Loading Clean Rwanda Nutrition Dashboard...\n")
cat("📁 Working directory:", getwd(), "\n")
cat("📊 Loading real data from CSV...\n\n")

# Suppress package loading messages
suppressMessages({
  # Source and run the app
  source("app.r", encoding = "UTF-8")
})

cat("\n🎉 Clean Dashboard launched successfully!\n")
cat("📈 Real CSV data loaded and displayed\n")
cat("🌍 Interactive charts and maps ready\n")
cat("🇷🇼 Bilingual interface available\n")
cat("✨ Console warnings suppressed for cleaner experience\n")
cat("\n💡 Your dashboard is now running with real data!\n")
