# 01_starling_native_calibration_shapefile_prep.R
#
# This script creates a shapefile for the native area for the Common starling.
# This covers the land masses likely accessible to the Common starling in the 
# native range. This includes the Azores in the Atlantic.
#
# Longitude extent: -40 to the Bering strait
# Latitude extent: 0 to 90
#
# This code was tested in R version 4.2.1 on NeSI.
#
# Load libraries ----------------------------------------------------#####
library(sf)  # read, write and deal with shapefiles. sf is replacing rgdal
library(tidyverse)  # data manipulation
# library(plotly) # some interactive plots for some basic visualisation
library(rgeos) # gUnaryUnion and gIntersect function for merging/clipping

# # Read in data ------------------------------------------------------#####
# # the basemaps area downloaded from:
# # https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/
# # version 5.1.1
# sf_admin <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
# unique(sf_admin$CONTINENT)
# # plot(basemapShp, xlim = c(0,180), ylim = c(0,60))  # Quick plot
# # basemapShp@data[basemapShp$CONTINENT == "Asia",] %>% View()  # Quick visualisation of countries
# 
# # Subset relevant continents ----------------------------------------#####
# ls_continents <- c("Asia", "Europe", "Africa")
# sf_relaventContinents <- sf_admin %>% 
#   filter(CONTINENT %in% ls_continents)
# # plot(st_geometry(sf_relaventContinents))
# 
# # Merge countries into one polygon ----------------------------------#####
# sf::sf_use_s2(F)  # Had to turn this off for the st_union function
# # Crop out areas South of the equator
# sf_cropped <- st_crop(sf_relaventContinents, y = c(xmin = -180, xmax = 180, ymin = 0, ymax = 90))
# # Divide geometry feature to multipolygon to polygon
# # Used to have only one feature for multiple polygons (france for Europe france
# # and French Guyana)
# sf_croppedPoly <- st_cast(sf_cropped,"MULTIPOLYGON") %>%
#   st_cast("POLYGON")
# # Determine which polygon is the "European" countries in S. America
# sf_croppedRemov <- st_crop(sf_croppedPoly, y = c(xmin = -100, xmax = -40, ymin = 0, ymax = 90))
# # sf_croppedRemov %>% View()
# 
# # plot(st_geometry(sf_croppedPoly))
# # plot(st_geometry(sf_croppedRemov), add = T, col = "red")
# # Filter out polygons (French Guyana and Netherlands overseas territory)
# sf_croppedPoly$rownumber <- rownames(sf_croppedPoly)
# sf_croppedReal <- sf_croppedPoly %>% 
#   filter(!rownumber %in% rownames(sf_croppedRemov))
# # Check if all countries 
# # plot(st_geometry(sf_croppedReal), col = "red")
# print("Merge countries into one polygon")
# sf_poly <- st_union(sf_croppedReal)
# # Crop off the very tip of the Russia and the berring strait to make 
# # things easier to deal with
# print("Crop off very tip of Russia pass 180 latitude")
# sf_poly <- st_crop(sf_poly, y = c(xmin = -30, xmax = 180, ymin = 0, ymax = 90))
sf_poly <- st_read(dsn = path.expand("data/starling/native_calibration_area/starling_native_plot_area.shp"))

# Quick plot 
# plot(st_geometry(sf_poly))
#
# # Alter projection slightly for plotting
# target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=80")
# 
# # Transform and plot
# sf_poly2 <- sf_poly %>% st_transform(crs = target_crs)
# plot(st_geometry(sf_poly2), col = "red")

# See https://gist.github.com/valentinitnelav/c7598fcfc8e53658f66feea9d3bafb40 
# for 
# Plotting a continuous Russia to the berring strait without the date line

# Choice of calibration area ----------------------------------------#####
# The calibration area is defined based on the accessible areas to starlings
# from the native range. These are based on the starling records and
# also the data from the ebird.
# 
# The ebird data is quite large and therefore were not plotted as the
# birdlife int. shapefile is sufficient
#
# Plot with distribution from birdlife map --------------------------#####
print("Read birdlife internation starling distribution")
sf_dist <- st_read(dsn = path.expand("data/starling/S_vulgaris_distribution_map/data_0.shp"))
sf_starNat <- st_union(sf_dist[c(1,2),])
sf_buf <- st_buffer(sf_starNat, dist = 500000)
# sf_buf1500 <- st_buffer(sf_starNat, dist = 1500000)
sf_bufclip <- st_intersection(sf_buf, sf_poly)
# sf_buf1500clip <- st_intersection(sf_buf1500, sf_poly)
sf_buf1500clip <- st_read(dsn = path.expand("data/starling/native_calibration_area/starling_native_calibration_area.shp"))
pts_PolygonCorner <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(150, 55,      # allow clipping out some islands
    66, 69,
    30, 70,
    30, 81,
    -30, 81,
    -30, 13,
    79, 5,
    105, 5,
    125, 23,
    150, 40,
    150, 55),
  ncol = 2, byrow = TRUE
)
sf_polyCorner <- st_polygon(x = list(pts_PolygonCorner)) %>%
  st_sfc(crs = st_crs(sf_poly))
sf::sf_use_s2(TRUE)

sf_mancal <- st_intersection(sf_poly, sf_polyCorner)

png("results/starling/potential_calibration_area_maps.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), col = "lightgrey", axes = F)
plot(st_geometry(sf_mancal), add = T, col = "darkgrey")
plot(st_geometry(sf_buf1500clip), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_bufclip), col = "orange", add = T)
plot(st_geometry(sf_starNat), col = "green", add = T)
box()
legend(x = 140, y = 40, 
       legend = c("Breeding range", "500km buffer", "1500km buffer", "Cal. area option 1", "Cal. area option 2"), 
       fill = c("green", "orange", alpha("red", 0.4), "darkgrey", "lightgrey"))
dev.off()
# 
# # Write shapefile of calibration area -------------------------------
dir.create("data/starling/native_calibration_area", showWarnings = F, recursive = T)
st_write(sf_buf1500clip, "data/starling/native_calibration_area/starling_native_calibration_area.shp", delete_dsn = T)
st_write(sf_poly, "data/starling/native_calibration_area/starling_native_plot_area.shp", delete_dsn = T)

# Re-read output shapefile and plot to PNG file ---------------------#####
sf_buf1500clip <- st_read(dsn = path.expand("data/starling/native_calibration_area/starling_native_calibration_area.shp"))
# Make a quick plot with the plot function, transform the coordinates slightly
# by shifting the coordinates by 80 degrees longitude
# target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=80")
source("scripts/patternfun.R")

png("data/starling/native_calibration_area/starling_native_calibration_area_shapefile.png", width = 11.7, height = 8.3, res = 450, units = "in")
patternLayer(x = sf_buf1500clip, pattern = "diamond", density = 0.8)
plot(st_geometry(sf_buf1500clip), add = T)
plot(st_geometry(sf_poly), axes = F, add = T)
box()
# plot(st_geometry(sf_buf1500clip))
plot(st_geometry(sf_starNat), col = alpha("darkgreen", 0.4), add = T)
axis(1)
axis(2)
dev.off()


