library(dplyr)
# R/data_utils.R

# ==============================================================================
# 🔹 ENHANCED DATA GENERATION (Based on Real DHS Patterns)
# ==============================================================================

create_enhanced_rwanda_data <- function() {
  # Real Rwanda districts (all 30 districts)
  districts <- c(
    "Gasabo", "Nyarugenge", "Kicukiro",  # City of Kigali
    "Nyanza", "Gisagara", "Nyaruguru", "Huye", "Nyamagabe", "Ruhango", "Muhanga", "Kamonyi",  # Southern Province
    "Karongi", "Rutsiro", "Rubavu", "Nyabihu", "Ngororero", "Rusizi", "Nyamasheke",  # Western Province
    "Musanze", "Burera", "Gakenke", "Rulindo", "Gicumbi",  # Northern Province
    "Rwamagana", "Nyagatare", "Gatsibo", "Kayonza", "Kirehe", "Ngoma", "Bugesera"  # Eastern Province
  )
  
  provinces <- c(
    rep("City of Kigali", 3),
    rep("Southern Province", 8),
    rep("Western Province", 7),
    rep("Northern Province", 5),
    rep("Eastern Province", 7)
  )
  
  # Years based on DHS surveys
  years <- seq(2000, 2020, by = 1)
  
  # Create district coordinates (approximate)
  district_coords <- data.frame(
    district = districts,
    province = provinces,
    lat = c(-1.925, -1.948, -1.970, -2.348, -2.668, -2.583, -2.596, -2.421, -2.107, -2.084, -2.327, -2.206, -2.000, -1.700, -1.648, -1.711, -2.439, -2.261, -1.499, -1.517, -1.677, -1.864, -1.433, -1.950, -1.300, -1.350, -1.850, -2.150, -2.150, -2.200),
    lon = c(30.097, 30.059, 30.099, 29.739, 29.670, 29.398, 29.738, 29.126, 29.633, 29.754, 29.574, 29.246, 29.339, 29.281, 29.509, 29.691, 28.904, 29.138, 29.636, 29.758, 29.760, 29.539, 29.583, 30.435, 30.325, 30.375, 30.450, 30.650, 30.650, 30.100)
  )
  
  # Generate full dataset
  set.seed(123)
  full_data <- expand.grid(
    district = districts,
    year = years,
    stringsAsFactors = FALSE
  )
  
  # Add province information
  full_data <- merge(full_data, district_coords, by = "district", all.x = TRUE)
  
  # Generate realistic nutrition indicators based on DHS trends
  full_data <- full_data %>%
    mutate(
      # Stunting rates (declining trend from ~50% in 2000 to ~33% in 2020)
      stunting = pmax(15, 50 - (.data$year - 2000) * 0.8 + rnorm(nrow(full_data), 0, 3) +
                     case_when(
                       .data$province == "City of Kigali" ~ -8,
                       .data$province == "Southern Province" ~ -2,
                       .data$province == "Western Province" ~ 3,
                       .data$province == "Northern Province" ~ 0,
                       .data$province == "Eastern Province" ~ 2
                     )),
      
      # Wasting rates (relatively stable ~3-7%)
      wasting = pmax(1, 5 + rnorm(nrow(full_data), 0, 2) +
                    case_when(
                      .data$province == "Eastern Province" ~ 2,
                      TRUE ~ 0
                    )),
      
      # Anemia rates (declining trend from ~55% to ~36%)
      anemia = pmax(20, 55 - (.data$year - 2000) * 0.9 + rnorm(nrow(full_data), 0, 4) +
                   case_when(
                     .data$province == "City of Kigali" ~ -5,
                     .data$province == "Western Province" ~ 3,
                     TRUE ~ 0
                   )),
      
      # Poverty rates (World Bank data trend)
      poverty_rate = pmax(10, 77 - (.data$year - 2000) * 2.3 + rnorm(nrow(full_data), 0, 5) +
                         case_when(
                           .data$province == "City of Kigali" ~ -15,
                           .data$province == "Eastern Province" ~ 5,
                           TRUE ~ 0
                         )),
      
      # Additional indicators
      maternal_education = pmax(0, 2 + (.data$year - 2000) * 0.3 + rnorm(nrow(full_data), 0, 1.5)),
      sanitation_access = pmin(95, 30 + (.data$year - 2000) * 2.5 + rnorm(nrow(full_data), 0, 5)),
      food_diversity = pmin(9, 3.5 + (.data$year - 2000) * 0.15 + rnorm(nrow(full_data), 0, 0.5)),
      health_access_km = pmax(0.5, 8 - (.data$year - 2000) * 0.2 + rnorm(nrow(full_data), 0, 2)),
      
      # Additional context variables
      gender = sample(c("Male", "Female"), nrow(full_data), replace = TRUE),
      funding_source = sample(c("Government", "UNICEF", "World Bank", "USAID", "EU", "Private"), 
                             nrow(full_data), replace = TRUE, prob = c(0.3, 0.2, 0.15, 0.15, 0.1, 0.1)),
      top_drivers = sample(c("Food Insecurity", "Poor Sanitation", "Limited Healthcare", 
                           "Poverty", "Low Education", "Climate Factors", "Conflict"),
                         nrow(full_data), replace = TRUE)
    )
  
  return(full_data)
}

# Create regional comparison data (East African countries)
create_regional_data <- function() {
  countries <- c("Rwanda", "Uganda", "Kenya", "Tanzania", "Burundi", "DRC")
  years <- seq(2000, 2020, by = 5)
  
  set.seed(456)
  regional_data <- expand.grid(
    country = countries,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      stunting = case_when(
        .data$country == "Rwanda" ~ 50 - (.data$year - 2000) * 0.8 + rnorm(n(), 0, 2),
        .data$country == "Uganda" ~ 45 - (.data$year - 2000) * 0.5 + rnorm(n(), 0, 3),
        .data$country == "Kenya" ~ 42 - (.data$year - 2000) * 0.6 + rnorm(n(), 0, 2),
        .data$country == "Tanzania" ~ 48 - (.data$year - 2000) * 0.7 + rnorm(n(), 0, 3),
        .data$country == "Burundi" ~ 55 - (.data$year - 2000) * 0.4 + rnorm(n(), 0, 4),
        .data$country == "DRC" ~ 52 - (.data$year - 2000) * 0.3 + rnorm(n(), 0, 5)
      ),
      wasting = case_when(
        .data$country == "Rwanda" ~ 5 + rnorm(n(), 0, 1),
        .data$country == "Uganda" ~ 7 + rnorm(n(), 0, 2),
        .data$country == "Kenya" ~ 6 + rnorm(n(), 0, 1.5),
        .data$country == "Tanzania" ~ 8 + rnorm(n(), 0, 2),
        .data$country == "Burundi" ~ 9 + rnorm(n(), 0, 2.5),
        .data$country == "DRC" ~ 12 + rnorm(n(), 0, 3)
      )
    )
  
  return(regional_data)
}

# A more detailed simulation function
simulate_full_dataset <- function() {
  set.seed(42)
  
  districts <- c(
    "Gasabo", "Kicukiro", "Nyarugenge", "Bugesera", "Gatsibo", "Kayonza",
    "Kirehe", "Ngoma", "Nyagatare", "Rwamagana", "Burera", "Gakenke",
    "Gicumbi", "Musanze", "Rulindo", "Gisagara", "Huye", "Kamonyi",
    "Muhanga", "Nyamagabe", "Nyanza", "Nyaruguru", "Ruhango",
    "Karongi", "Ngororero", "Nyabihu", "Nyamasheke", "Rubavu",
    "Rutsiro", "Rusizi"
  )
  
  provinces <- c(
    rep("City of Kigali", 3), rep("Eastern Province", 7), rep("Northern Province", 5),
    rep("Southern Province", 8), rep("Western Province", 7)
  )
  
  # Correctly map provinces to districts
  district_province_map <- data.frame(district = districts, province = provinces)
  
  df <- expand.grid(
    district = districts,
    year = 2000:2020,
    gender = c("Female-led Household", "Male-led Household"),
    stringsAsFactors = FALSE
  )
  
  df <- df %>% left_join(district_province_map, by = "district")
  
  district_effects <- tibble(
    district = districts,
    re_stunting = rnorm(length(districts), 0, 5),
    re_poverty  = rnorm(length(districts), 0, 8)
  )
  
  df <- df %>% left_join(district_effects, by = "district")
  
  df <- df %>%
    mutate(
      stunting = round(pmax(15, 38 - (.data$year - 2000) * 0.9 + .data$re_stunting +
                               rnorm(n(), 0, 3) + ifelse(.data$gender == "Female-led Household", 2, -2)), 1),
      wasting  = round(pmax(2, 9 - (.data$year - 2000) * 0.3 + .data$re_stunting / 2 + rnorm(n(), 0, 1.5)), 1),
      anemia   = round(pmax(20, 42 - (.data$year - 2000) * 0.7 + .data$re_stunting / 1.5 + rnorm(n(), 0, 4)), 1),
      poverty_rate = round(pmax(20, 55 - .data$re_stunting + .data$re_poverty - (.data$year - 2000) * 1.0 + rnorm(n(), 0, 5)), 1),
      maternal_education = round(pmax(3, 5 + (.data$year - 2000) * 0.1 - .data$re_poverty / 10 + rnorm(n(), 0, 0.5)), 1),
      food_diversity     = round(pmax(2, 9 - .data$stunting / 8 - .data$poverty_rate / 20 + rnorm(n(), 0, 1)), 1),
      sanitation_access  = round(pmin(95, 60 + (.data$year - 2000) * 1.2 - .data$re_poverty + rnorm(n(), 0, 6)), 1),
      health_access_km   = round(pmax(1, 8 - .data$sanitation_access / 20 + .data$poverty_rate / 15 + rnorm(n(), 0, 1)), 1),
      funding_source     = sample(c("Government", "NGO", "Private Sector", "Community"), n(), replace = TRUE,
                                  prob = c(0.4, 0.3, 0.1, 0.2)),
      intervention_type  = sample(c("Agriculture", "Health", "Education", "WASH"), n(), replace = TRUE,
                                  prob = c(0.35, 0.3, 0.2, 0.15)),
      lat = round(-2.0 + (as.numeric(factor(.data$district)) - 15) * 0.05 + rnorm(n(), 0, 0.01), 4),
      lon = round(29.8 + (as.numeric(factor(.data$district)) %% 5 - 2) * 0.2 + rnorm(n(), 0, 0.01), 4)
    ) %>%
    dplyr::select(-starts_with("re_"))
  
  df$top_drivers <- mapply(function(p, m, f) {
    drivers <- c()
    if (p > 50) drivers <- c(drivers, "Poverty")
    if (m < 5) drivers <- c(drivers, "Maternal Education")
    if (f < 4) drivers <- c(drivers, "Food Diversity")
    if (length(drivers) == 0) return("Multiple Factors")
    paste(drivers, collapse = ", ")
  }, df$poverty_rate, df$maternal_education, df$food_diversity)
  
  return(df)
}
