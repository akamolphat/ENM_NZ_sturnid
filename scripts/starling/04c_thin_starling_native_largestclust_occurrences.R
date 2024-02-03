# 04c_thin_starling_native_largestclust_occurrences.R
# Load prerequisite libraries ---------------------------------------
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset.
library(sf)

# Reads in native occurrence points recurring localities ----------------
# Note that the work flow is a little different from the myna 
# dataset. In the myna work flow, the definition of recurring localities
# was done in the 04*_thin_myna_*.R scripts, while the same process
# was done in the 03*i_further_clean_*_starling_occurrences.R script
# This was to compartmentalise the process.
#
df_cellWithstarling <- fread("data/starling/filtered_occurrences/Starling_native_breeding_1971-2020_32km4yr_recurring_cellwithStarling.csv")

sf_clippedRegion <- st_read(dsn= path.expand("data/starling/native_calibration_area/starling_native_calibration_area.shp"))

# Thin dataset ------------------------------------------------------
# Thin per 32km cluster
dir.create("data/starling/filtered_occurrences/native_thinned/32km", recursive = T, showWarnings = F)
# Thin just the largest cluster again
clust <- 1
nkm <- 32
outFile <- paste("TEST_starling_native_32kmthinned_clust", sprintf("%04d", clust), sep = "")
print(outFile)
spThin::thin(df_cellWithstarling[df_cellWithstarling$Clust32km == clust,],
             lat.col = "y",
             long.col = "x",
             spec.col = "species",
             thin.par = nkm,  # 16km as this is the supposed home range
             reps = 1,
             locs.thinned.list.return = F,  # False so that the records are written into file directly
             write.files = TRUE,
             max.files = 1,
             out.dir = "data/starling/filtered_occurrences/native_thinned/32km/",
             out.base = outFile,
             write.log.file = F)
