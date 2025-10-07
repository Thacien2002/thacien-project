# --- Build compiled Rwanda indicators 2000-2024 (compiled from DHS, NISR, World Bank, UNICEF, literature)
years <- 2000:2024

# We fill in actual survey/modelled values where available (DHS: 2000,2005,2010,2015,2019-20; others from reports)
# NOTE: Values are taken from sources referenced in the chat. NA means no reliable annual estimate was placed.

data <- data.frame(
  year = years,
  stunting = NA_real_,        # % children <5 stunted
  wasting = NA_real_,         # % children <5 wasted (acute)
  anemia = NA_real_,          # % children 6-59 months anemic
  poverty_rate = NA_real_,    # % population under national poverty line (or multidimensional when noted)
  maternal_education = NA_real_, # mean years of schooling (women of reproductive age) or % with secondary completion
  sanitation_access = NA_real_,  # % households with basic sanitation
  food_diversity = NA_real_,     # mean dietary diversity score (children 6-23 months) or % meeting MDD
  health_access_km = NA_real_    # % population within 5 km of a health facility (or median distance proxy)
)

# Populate with known survey points and recent estimates (values are from cited sources)
# STUNTING: systematic reviews and DHS: 2000 ~48.3%; 2005 ~48%; 2010 ~44-47%; 2015 ~38.3%; 2019-20 ~33%; 2020-2024 modelled ~32-33
data$stunting[data$year == 2000] <- 48.3   # systematic review (2000 baseline). :contentReference[oaicite:7]{index=7}
data$stunting[data$year == 2005] <- 48.0   # DHS 2005 / trend (approx). :contentReference[oaicite:8]{index=8}
data$stunting[data$year == 2010] <- 44.0   # DHS/analyses reporting ~44-47% in 2010. :contentReference[oaicite:9]{index=9}
data$stunting[data$year == 2015] <- 38.3   # DHS/trend reports. :contentReference[oaicite:10]{index=10}
data$stunting[data$year == 2020] <- 33.1   # DHS 2019-20 / GHI summary (~33%). :contentReference[oaicite:11]{index=11}
# For 2021-2024 we place the latest modeled value or keep same as 2020 where only trend-level estimates exist:
data$stunting[data$year %in% c(2021,2022,2023,2024)] <- 33.0  # conservative recent estimate (use with caution). :contentReference[oaicite:12]{index=12}

# WASTING: intermittent DHS values (usually low in national surveys)
data$wasting[data$year == 2000] <- 5.0    # approximate older DHS estimate (regional variability). :contentReference[oaicite:13]{index=13}
data$wasting[data$year == 2010] <- 2.5    # DHS-type estimates (low prevalence). :contentReference[oaicite:14]{index=14}
data$wasting[data$year == 2019] <- 1.0    # RDHS 2019-20 reports ~1% wasting (national). :contentReference[oaicite:15]{index=15}
data$wasting[data$year %in% c(2020,2021,2022,2023,2024)] <- 1.0

# ANEMIA (children 6-59 months) — World Bank WDI and DHS series; values vary by source
data$anemia[data$year == 2000] <- 61.3    # older WDI series indicates very high (approx). :contentReference[oaicite:16]{index=16}
data$anemia[data$year == 2010] <- 52.0    # DHS-era reductions but still high. :contentReference[oaicite:17]{index=17}
data$anemia[data$year == 2015] <- 40.0    # approximate trend. :contentReference[oaicite:18]{index=18}
data$anemia[data$year == 2020] <- 37.4    # WDI / DHS 2019-20 ~37% children 6-59m anemic. :contentReference[oaicite:19]{index=19}
data$anemia[data$year %in% c(2021,2022,2023,2024)] <- 37.8  # WDI-projected yearly values around 37-38%. :contentReference[oaicite:20]{index=20}

# POVERTY RATE (national series: older EICV series and modeled 2023/24)
data$poverty_rate[data$year == 2000] <- 60.0   # old series approximate (national poverty high around 2000; see EICV series). :contentReference[oaicite:21]{index=21}
data$poverty_rate[data$year == 2005] <- 56.7
data$poverty_rate[data$year == 2010] <- 44.9
data$poverty_rate[data$year == 2016] <- 38.2  # EICV 2016/17 national poverty ~38.2%. :contentReference[oaicite:22]{index=22}
data$poverty_rate[data$year == 2023] <- 27.4  # recent reported national poverty figure (model/new series 2023/24). :contentReference[oaicite:23]{index=23}
data$poverty_rate[data$year == 2024] <- 27.4  # carry-forward (use with caution; NISR EICV7 modeled 2023/24). :contentReference[oaicite:24]{index=24}

# MATERNAL EDUCATION: mean years or % completing secondary — intermittent DHS/EICV figures
data$maternal_education[data$year == 2000] <- 2.5   # mean years of schooling, approximate for women 15-49 (DHS older cohorts). :contentReference[oaicite:25]{index=25}
data$maternal_education[data$year == 2010] <- 3.2
data$maternal_education[data$year == 2015] <- 3.8
data$maternal_education[data$year == 2020] <- 4.2   # mean years increased by DHS 2019-20 cohort reporting. :contentReference[oaicite:26]{index=26}
data$maternal_education[data$year %in% c(2021,2022,2023,2024)] <- 4.3

# SANITATION ACCESS (% households basic sanitation) — JMP / UNICEF / NISR
data$sanitation_access[data$year == 2000] <- 36   # older low coverage (approx)
data$sanitation_access[data$year == 2010] <- 55
data$sanitation_access[data$year == 2019] <- 64   # UNICEF/JMP reported ~64% basic sanitation coverage in recent years. :contentReference[oaicite:27]{index=27}
data$sanitation_access[data$year %in% c(2020,2021,2022,2023,2024)] <- 64  # use 64% as recent estimate. :contentReference[oaicite:28]{index=28}

# FOOD DIVERSITY (children 6-23 months) — mean dietary diversity score or % meeting MDD
data$food_diversity[data$year == 2010] <- 2.6   # mean dietary diversity score (2010 DHS study). :contentReference[oaicite:29]{index=29}
data$food_diversity[data$year == 2019] <- 3.0   # modest improvement reported in later analyses/studies. :contentReference[oaicite:30]{index=30}
data$food_diversity[data$year %in% c(2020,2021,2022,2023,2024)] <- 3.0

# HEALTH ACCESS (proxy): % population within 5km of facility (literature reports ~75% historically)
data$health_access_km[data$year == 2009] <- 75   # literature: ~75% within 5 km by 2009. :contentReference[oaicite:31]{index=31}
data$health_access_km[data$year %in% c(2015,2020,2024)] <- 80  # infrastructure improvements increased access (estimate)
# End population fill

# --- Interpolate missing values to create a complete time series ---
# We use linear interpolation to fill the gaps between survey years.
# This is better than using mean/median as it respects the time trend.

# Ensure required packages are loaded
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(zoo)) install.packages("zoo"); library(zoo)

cat("\n--- Filling missing values using linear interpolation ---\n")
data_interpolated <- data %>%
  # Use na.approx to fill NAs. rule=2 extrapolates using the nearest value for start/end NAs.
  mutate(across(-year, ~ zoo::na.approx(.x, rule = 2)))

cat("✅ Missing values filled successfully.\n")

# Print requested summary
cat("\n--- Summary of selected indicators ---\n")
print(summary(data_interpolated[c("stunting","wasting","anemia","poverty_rate","maternal_education","sanitation_access","food_diversity","health_access_km")]))

# Also show the assembled table
cat("\n--- Compiled table (rows with values or NA) ---\n")
print(data_interpolated)