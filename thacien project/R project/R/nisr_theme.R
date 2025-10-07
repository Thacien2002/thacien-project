# Load required libraries with error handling
if (!require(fresh, quietly = TRUE)) {
  stop("Package 'fresh' is required but not installed. Please install it with: install.packages('fresh')")
}

if (!require(bslib, quietly = TRUE)) {
  stop("Package 'bslib' is required but not installed. Please install it with: install.packages('bslib')")
}

if (!require(htmltools, quietly = TRUE)) {
  stop("Package 'htmltools' is required but not installed. Please install it with: install.packages('htmltools')")
}

# ==============================================================================
# 🔹 CUSTOM THEME
# ==============================================================================
# This theme uses a custom color palette provided by the user.
#
# The font 'Nunito' is used for a clean and modern look.
# ==============================================================================

# Custom Colors
colors <- c("#034742", "#2ca02c", "#092CA0", "#431d85", "#570631", "#9e086f", "#87CEEB", "#add8e6", "#FFA500", "#1f77b4")

nisr_theme <- create_theme(
  
  # Define the core color palette
  adminlte_color(
    light_blue = colors[1],   # Custom color 1 (Primary)
    green = colors[2],       # Custom color 2 (Success)
    yellow = colors[9],      # Custom color 9 (Warning)
    red = "#d73925",         # A standard red for danger
    gray = "#f4f4f4",        # Light gray for backgrounds
    black = "#1a1a1a"         # Dark text for readability
  ),
  
  # Configure the sidebar for a professional look
  adminlte_sidebar(
    width = "250px",
    dark_bg = colors[1],       # Dark sidebar background using custom color 1
    dark_hover_bg = colors[3], # Blue on hover (using custom color 3)
    dark_color = "#ffffff"     # White text
  ),
  
  # Style dashboard boxes
  adminlte_vars(
    box_bg = "#ffffff",
    box_border_color = "#e3e6f0",
    box_collapsed_bg = "#f8f9fc",
    box_collapsed_color = "#1a1a1a"
  ),
  
  # Apply global CSS rules for fonts and other elements
  rules = list(
    HTML("
      /* Use Nunito font for a modern, clean aesthetic */
      body, h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
        font-family: 'Nunito', sans-serif;
      }

      /* Improve readability with a slightly larger base font size */
      body {
        font-size: 15px;
      }

      /* Style the main header logo */
      .main-header .logo {
        font-weight: 800;
        font-size: 22px;
      }

      /* Style the titles of dashboard boxes */
      .box-title {
        font-weight: 700;
        font-size: 19px;
      }
    ")
  )
)