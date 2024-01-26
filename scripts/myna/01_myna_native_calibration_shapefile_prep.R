# 01_myna_native_calibration_shapefile_prep.R
# 
# This script creates shapefiles for plotting the myna's native range
# and the myna's native range calibration area. 
#
# The calibration area tries to limit the area which is accessible to
# the myna in the native range, including the areas with range expansion
# in the past 100-200 years (e.g. SE Asia).
#
# Central-East China (including Hong Kong) and Korea were excluded as these 
# were recent introductions, and I did not want to include potential suitable 
# areas which the species have not yet established due to the dispersal 
# barriers
#
# Part 1. Create shapefile for plotting native range occurrences
# Part 2. Create shapefile for calibration area for the native range
#
# All calibration areas to be overlayed by thinned myna points
#
# Load libraries ----------------------------------------------------#####
library(sf)  # manipulating shapefiles
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)
# Part 1. Create shapefile for plotting -----------------------------#####
sf_admin <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
# Turn geometry (s2) off to allow st_union with no errors. Assumes planar
sf::sf_use_s2(FALSE)
sf_shorelines <- st_union(sf_admin)
plot(st_geometry(sf_shorelines))
# Crop to plotting extent
pts_plotExtent <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(45, 0,      # allow clipping out some islands
    45, 55,
    124, 55,
    124, 0,
    45, 0),
  ncol = 2, byrow = TRUE
)

sf_plotExtent <- st_polygon(x = list(pts_plotExtent)) %>%
  st_sfc(crs = st_crs(sf_shorelines))
sf_shorelinesclipped <- st_intersection(sf_plotExtent, sf_shorelines)
plot(sf_shorelinesclipped)
# Part 1.a. Output native plotting area to shapefile ----------------#####
st_write(sf_shorelinesclipped, "data/myna/native_plot_area/native_plot_area.shp")

# Part 2. Create native calibration area shapefile ------------------#####
# sf_admin <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
# Part 2.a. Subset relevant countries -------------------------------#####
countryList <- c("Kazakhstan", 
                 "Uzbekistan", 
                 "Tajikistan",
                 "Turkmenistan", 
                 "Afghanistan", 
                 "Iran", 
                 "Pakistan",
                 "Kashmir",
                 "Kyrgyzstan", 
                 "India", 
                 "Sri Lanka", 
                 "Bangladesh", 
                 "Nepal", 
                 "Bhutan", 
                 "Myanmar", 
                 "China", 
                 "Thailand", 
                 "Malaysia", 
                 "Singapore", 
                 "Cambodia", 
                 "Laos", 
                 "Vietnam",
                 # Russia and countries from this point onwards are 
                 # only included for completeness of the calibration area
                 "Russia",  
                 "Mongolia",
                 "Azerbaijan",
                 "Armenia",
                 "Georgia")

sf_adminNat <- sf_admin[sf_admin$SOVEREIGNT %in% countryList,]
# sf::sf_use_s2(FALSE)
sf_Nat <- st_union(sf_adminNat)
plot(st_geometry(sf_Nat))

# Part 2.b. Crop using plotting area for easier visualisation -------#####
sf_Nat <- st_intersection(sf_Nat, sf_shorelinesclipped)
plot(st_geometry(sf_Nat))

# Part 2.c. Add birdlife native distribution for visuatisation ------#####
sf_mynaDist <- st_read(dsn = path.expand("data/myna/A_tristis_distribution_map/data_0.shp"))
sf_mynaNat <- sf_mynaDist[1,]  # First feature is the native range polygon
plot(st_geometry(sf_Nat))
plot(st_geometry(sf_mynaNat), col = "green", add = T)

# Part 2.d. Add 500km buffer to native distribution for visualisation ####
sf::sf_use_s2(TRUE)
sf_buf <- st_buffer(sf_mynaNat, dist = 500000)
sf_bufclip <- st_intersection(sf_buf, sf_Nat)
plot(st_geometry(sf_Nat))
plot(st_geometry(sf_bufclip), col = "yellow", add = T)
plot(st_geometry(sf_mynaNat), col = "green", add = T)

# Part 2.e. Add occurrence points to visualise barriers (POSTHOC) ---#####

# Part 2.f. Draw extent manually ------------------------------------#####
pts_PolygonCorner <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(45, 0,      # allow clipping out some islands
    45, 55,
    90, 55,
    90, 31,
    98, 31,
    113, 20,
    113, 18,
    105, 0,
    45, 0),
  ncol = 2, byrow = TRUE
)
sf_polyCorner <- st_polygon(x = list(pts_PolygonCorner)) %>%
  st_sfc(crs = st_crs(sf_Nat))
sf_polyClip <- st_intersection(sf_polyCorner, sf_Nat)

png("data/myna/native_calibration_area/cal_area.png")
plot(st_geometry(sf_shorelinesclipped))
plot(st_geometry(sf_bufclip), col = "yellow", add = T)
plot(st_geometry(sf_mynaNat), col = "green", add = T)
plot(st_geometry(sf_polyClip), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3), alpha = 0.1, add = T)
legend(x = 95, y = 50, legend = c("Native range", "500km buffer", "Calibration area"),
       col = c("green", "yellow", rgb(red = 1, green = 0, blue = 0, alpha = 0.3)), pch = c(15,15,15))
dev.off()

# Part 2.g. Output calibration area shapefile -----------------------#####
st_write(sf_polyClip, "data/myna/native_calibration_area/native_calibration_area.shp")

# Re-read in shape file and quickly plot ----------------------------#####
sf_shorelinesclipped <- st_read(dsn= path.expand("data/myna/native_plot_area/native_plot_area.shp"))
sf_polyClip <- st_read(dsn= path.expand("data/myna/native_calibration_area/native_calibration_area.shp"))

# plot(st_geometry(sf_shorelinesclipped))
png("results/Native_area_shapefile.png", width = 11.7, height = 8.3, res = 450, units = "in")
plot(c(45,124),c(0, 55),type="n", main="Native calibration area", xlab="Longitude", ylab="Latitude", xaxs="i", yaxs="i", xlim = c(45, 124), asp = 1, axes = F)
plot(st_geometry(sf_polyClip), col = "green", border = "green", add = T)
plot(st_geometry(sf_shorelinesclipped), add = T)
axis(1, at = c(45, 70, 95, 120, 145))
axis(2, at = seq(0, 60, by = 15))
legend(x = 124, y = 55, xjust = 1, legend = "Native calibration area", fill = "green")
dev.off()
# Read in thinned native range dataset ------------------------------#####
# sf_mynaOccNat <- fread("data/filtered_occurrences/native_thinned/16km/myna_native_thinned_merged.csv")
# # Remove NA (1 point in clust0005 somehow)
# sf_mynaOccNat <- sf_mynaOccNat %>% drop_na()
# # Convert to sf
# sf_mynaOccNat <- st_as_sf(sf_mynaOccNat, coords = c("x","y"), remove = FALSE)
# # Quick plot
# plot(st_geometry(sf_coastline))
# plot(st_geometry(sf_mynaDist), col = c("green", "red"), add = T)
# plot(st_geometry(sf_mynaOccNat), add = T)





