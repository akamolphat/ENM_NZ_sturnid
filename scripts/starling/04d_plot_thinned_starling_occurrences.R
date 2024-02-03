# 04d_plot_thinned_starling_occurrences
#
# Load libraries ----------------------------------------------------#####
library(terra)
library(tidyverse)
library(data.table)
library(sf)

# Read in shapefiles ------------------------------------------------
sf_polyClip <- st_read(dsn= path.expand("data/starling/native_calibration_area/starling_native_calibration_area.shp"))
sf_coastline <- st_read(dsn = path.expand("data/ne_10m_coastline/ne_10m_coastline.shp"))
sf_country <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
sf_NZ <- st_read(dsn = path.expand("data/starling/NZ_calibration_area/starling_NZ_calibration_area.shp"))
# Read in points ----------------------------------------------------
sf_natOcc <- fread(file = "data/starling/filtered_occurrences/native_thinned/32km/starling_native_thinned_merged.csv") %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_coastline))
sf_NZOcc <- fread(file = "data/starling/filtered_occurrences/NZ_thinned/32km/starling_NZ_thinned_merged.csv") %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_coastline))


png("results/starling/occurrences/starling_native_thinned.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_polyClip), col = alpha("grey", 0.2), border = alpha("grey", 0.2))
plot(st_geometry(sf_country), lwd = 0.1, add = T)
plot(st_geometry(sf_natOcc), col = "black", add = T)
dev.off()

png("results/starling/occurrences/starling_NZ_thinned.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_NZ), col = alpha("grey", 0.2), border = alpha("grey", 0.2))
plot(st_geometry(sf_country), lwd = 0.1, add = T)
plot(st_geometry(sf_natOcc), col = "black", add = T)
dev.off()
