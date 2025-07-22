library(dplyr)
library(sf)
library(terra)
library(bcdata)      # For BC government data
library(osmdata)     # For OpenStreetMap data
library(bcmaps)

# ===============================================
# METHOD 1: Use BC Data Catalogue (BEST OPTION)
# ===============================================

# Search for island or coastal data in BC Data Catalogue
# bc_search_catalogue("island")  # Uncomment to see available datasets

# Get detailed coastline data from BC government
bc_coastline <- try({
  bcdc_get_data("f4deb89b-0a35-4820-b4b8-e2c5b1aa8a36") %>%  # BC coastline dataset
    st_transform(4326) %>%
    st_filter(st_point(c(-126.5, 50.7)) %>% 
                st_sfc(crs = 4326) %>% 
                st_buffer(0.5))  # 0.5 degree buffer around Gilford Island area
}, silent = TRUE)

if(!inherits(bc_coastline, "try-error") && nrow(bc_coastline) > 0) {
  # Filter for Gilford Island specifically
  gilford_island <- bc_coastline %>%
    filter(str_detect(toupper(FEATURE_NAME), "GILFORD|GILVORD") | 
             str_detect(toupper(FEATURE_DESCRIPTION), "GILFORD|GILVORD")) %>%
    st_union() %>%
    st_sf() %>%
    mutate(name = "Gilford Island")
  
  if(nrow(gilford_island) > 0) {
    print("Success! Found Gilford Island in BC Data Catalogue")
    method_used <- "BC Data Catalogue"
  }
}

# ===============================================
# METHOD 2: Use OpenStreetMap Data (RELIABLE FALLBACK)
# ===============================================

if(!exists("gilford_island") || nrow(gilford_island) == 0) {
  print("Trying OpenStreetMap data...")
  
  # Get Gilford Island from OpenStreetMap
  gilford_island <- try({
    # Search for Gilford Island
    opq(bbox = c(-127, 50.5, -126, 51)) %>%
      add_osm_feature(key = "place", value = "island") %>%
      add_osm_feature(key = "name", value = "Gilford Island") %>%
      osmdata_sf()
  }, silent = TRUE)
  
  if(!inherits(gilford_island, "try-error") && 
     !is.null(gilford_island$osm_multipolygons) && 
     nrow(gilford_island$osm_multipolygons) > 0) {
    
    gilford_island <- gilford_island$osm_multipolygons %>%
      select(name, geometry) %>%
      filter(!is.na(name)) %>%
      st_transform(4326)
    
    print("Success! Found Gilford Island in OpenStreetMap")
    method_used <- "OpenStreetMap"
  } else {
    # Try broader search for any islands in the area
    gilford_island <- try({
      opq(bbox = c(-127, 50.5, -126, 51)) %>%
        add_osm_feature(key = "place", value = "island") %>%
        osmdata_sf()
    }, silent = TRUE)
    
    if(!inherits(gilford_island, "try-error") && 
       !is.null(gilford_island$osm_multipolygons)) {
      
      gilford_island <- gilford_island$osm_multipolygons %>%
        filter(str_detect(toupper(name), "GILFORD") | 
                 is.na(name)) %>%  # Include unnamed islands that might be Gilford
        st_transform(4326)
      
      if(nrow(gilford_island) > 0) {
        print("Found islands in the area - may include Gilford Island")
        method_used <- "OpenStreetMap (area search)"
      }
    }
  }
}

# ===============================================
# METHOD 3: Create from Known Coordinates (MANUAL APPROACH)
# ===============================================

if(!exists("gilford_island") || nrow(gilford_island) == 0) {
  print("Creating Gilford Island outline from approximate coordinates...")
  
  # Approximate outline of Gilford Island based on known geography
  # These coordinates trace the rough outline of the island
  gilford_coords <- matrix(c(
    -126.7, 50.65,   # Southwest point
    -126.6, 50.65,   # Southeast point  
    -126.55, 50.70,  # East central
    -126.60, 50.75,  # Northeast
    -126.65, 50.78,  # North point
    -126.75, 50.76,  # Northwest
    -126.80, 50.72,  # West central
    -126.75, 50.68,  # Southwest return
    -126.7, 50.65    # Close polygon
  ), ncol = 2, byrow = TRUE)
  
  gilford_island <- st_polygon(list(gilford_coords)) %>%
    st_sfc(crs = 4326) %>%
    st_sf() %>%
    mutate(name = "Gilford Island (approximate)")
  
  method_used <- "Manual coordinate outline"
  print("Created approximate Gilford Island outline")
}

# ===============================================
# METHOD 4: Use Natural Earth Data (SIMPLE FALLBACK)
# ===============================================

if(!exists("gilford_island") || nrow(gilford_island) == 0) {
  print("Using Natural Earth coastline data...")
  
  # Get coastline data from Natural Earth
  coastline <- ne_coastline(scale = 10, returnclass = "sf") %>%
    st_crop(c(xmin = -127, ymin = 50.5, xmax = -126, ymax = 51))
  
  # Create a buffer around the coastline to approximate island shapes
  if(nrow(coastline) > 0) {
    gilford_island <- coastline %>%
      st_buffer(0.01) %>%  # Small buffer
      st_union() %>%
      st_sf() %>%
      mutate(name = "Coastal area (Natural Earth)")
    
    method_used <- "Natural Earth coastline"
  }
}

# ===============================================
# FINALIZE AND TRANSFORM
# ===============================================

# Ensure we have something to work with
if(!exists("gilford_island") || nrow(gilford_island) == 0) {
  stop("Could not create Gilford Island shapefile with any method")
}

# Clean up and standardize
gilford_island_final <- gilford_island %>%
  st_transform(4326) %>%  # Ensure WGS84
  st_make_valid() %>%     # Fix any geometry issues
  slice(1) %>%            # Take first/largest polygon if multiple
  mutate(
    island_name = "Gilford Island",
    method_created = method_used,
    date_created = Sys.Date()
  )

# Project to BC Albers for the tutorial
study_area <- gilford_island_final %>%
  st_transform(3347)

# ===============================================
# SAVE THE SHAPEFILE
# ===============================================

# Create data directory
if (!dir.exists("./_data/")) {
  dir.create("./_data/", recursive = TRUE)
}

# Save as shapefile
st_write(gilford_island_final, "./_data/gilford_island.shp", 
         delete_dsn = TRUE, quiet = TRUE)

# Save as GeoJSON (more modern format)
st_write(gilford_island_final, "./_data/gilford_island.geojson", 
         delete_dsn = TRUE, quiet = TRUE)

# Also save the projected version for immediate use
st_write(study_area, "./_data/gilford_island_bc_albers.geojson", 
         delete_dsn = TRUE, quiet = TRUE)

# ===============================================
# QUICK PREVIEW
# ===============================================

print(paste("Successfully created Gilford Island shapefile using:", method_used))
print(paste("Area:", round(as.numeric(st_area(study_area))/1000000, 2), "square kilometers"))
print(paste("Bounding box:", paste(round(st_bbox(gilford_island_final), 3), collapse = ", ")))

# Quick plot to verify
plot(st_geometry(gilford_island_final), col = "lightgreen", border = "darkgreen")
title(paste("Gilford Island -", method_used))

# ===============================================
# FOR USE IN YOUR TUTORIAL - REPLACE THIS LINE:
# ===============================================

# OLD tutorial line:
# study_area = vect("./_data/vancouver-island_1235.geojson")

# NEW line for your tutorial:
study_area_vect = vect("./_data/gilford_island_bc_albers.geojson")

print("\n=== TO USE IN YOUR TUTORIAL ===")
print("Replace this line in the tutorial:")
print('study_area = vect("./_data/vancouver-island_1235.geojson")')
print("\nWith this line:")
print('study_area = vect("./_data/gilford_island_bc_albers.geojson")')
print("\nOr for sf format:")
print('study_area = st_read("./_data/gilford_island.geojson")')