# 01b_starling_NZ_shapefile_prep.R
#
# This script creates shapefiles for plotting the myna's NZ range
# and the myna's NZ range calibration area. 
# 
# As mynas have been introduced in both the North and South Island,
# Both the islands will be included and therefore the plotting and 
# the calibration area are the same
#
# The original script used rgdal package to deal with shapefiles but 
# the rgdal package will be superseded at the end of 2023 and therefore
# the script was altered to use the sf package instead.
#
# Load libraries ----------------------------------------------------#####
library(sf)  # manipulating shapefiles
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)

# Read in data ------------------------------------------------------#####
# the basemaps area downloaded from:
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/
# version 5.1.1
sf_admin <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
# plot(st_geometry(sf_admin), xlim = c(166,179), ylim = c(-47,-34))  # Quick plot

# Clip polygon using polygon mask -----------------------------------#####
# Define polygon to be used for clipping out some NZ offshore islands
# Define polygon corners
pts_plotExtent <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(166, -46.25,      # allow clipping out some islands
    166, -34.25,
    179, -34.25,
    179, -47,
    169, -46.75,
    166, -46.25),
  ncol = 2, byrow = TRUE)

sf_plotExtent <- st_polygon(x = list(pts_plotExtent)) %>%
  st_sfc(crs = st_crs(sf_admin))
sf::sf_use_s2(FALSE)
sf_NZclipped <- st_intersection(sf_plotExtent, sf_admin)
# plot(st_geometry(sf_NZclipped))

# Write shapefile of NZ ---------------------------------------------#####
dir.create("data/starling/NZ_calibration_area/", recursive = T, showWarnings = F)
st_write(sf_NZclipped, "data/starling/NZ_calibration_area/starling_NZ_calibration_area.shp")

