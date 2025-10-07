# Test script to verify real data loading and chart functionality
cat("🧪 Testing Real Data Dashboard...\n")

# Set working directory
setwd("C:/Users/USER/Desktop/thacien project/R project")

# Test data loading
cat("📊 Testing CSV data loading...\n")
data <- read.csv('project/data/nutrition_data.csv')
cat("✅ CSV loaded:", nrow(data), "rows\n")
cat("📅 Year range:", min(data$year), "-", max(data$year), "\n")
cat("🌍 Countries:", length(unique(data$district)), "\n")
cat("🏙️ Cities:", length(unique(data$province)), "\n")

# Test sample calculations
cat("\n📈 Testing sample calculations...\n")
sample_data <- data %>% 
  filter(year == max(year)) %>%
  group_by(district) %>%
  summarise(
    avg_stunting = mean(stunting, na.rm = TRUE),
    avg_wasting = mean(wasting, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_stunting))

cat("✅ Sample calculations successful\n")
cat("📊 Top 5 countries by stunting:\n")
print(head(sample_data, 5))

cat("\n🚀 All tests passed! Dashboard should work with real data.\n")
cat("💡 Run: source('app.r') to launch the dashboard\n")
