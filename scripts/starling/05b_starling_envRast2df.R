# This script creates a dataframe with gridcells from the entire
# native calibration area. Grid cells with presence record is marked as 1.
#
# The presence record is based on the spatially thinned occurrence record
# based on the home range.
# 
# The merging is done separately for the native and NZ range for
# compartmentalisation
#
# Load libraries ----------------------------------------------------#####
library(terra)
library(tidyverse)
library(data.table)
orig_round <- getNumericRounding()
setNumericRounding(2)  # This allows merging and avoid limitations with
# floating point numbers. 2 bytes rounding equates to approximately
# 11s.f. which is plenty good enough
# Define var names --------------------------------------------------#####
# This dataset is very big so we are only getting values from the
# variables of interest for humboldt
ls_var <- c("bio12", "gsl", "gdd5", "tree_cover_pct", "pop_dens_2000")
# ls_var <- c(paste("bio", seq(1, 19), sep = ""),                          # CHELSA bioclim var
#             "gsl", "gst", "gsp", "gdd5",                                 # growing season var
#             "tree_cover_pct",
#             "urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled", "log_pop_dens")  # human var
# Create folder to store outputs ------------------------------------
dir.create(path = "data/starling/env_var_df", recursive = T, showWarnings = F)
# Native area -------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
r_native <- terra::rast(paste("data/starling/native_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))

## Convert SpatRast to dataframe ------------------------------------#####
df_native <- setDT(terra::as.data.frame(r_native, na.rm = T, xy = T))
df_native$x <- round(df_native$x, 5)
df_native$y <- round(df_native$y, 5)
# Read in occurrences -----------------------------------------------#####
df_natOcc <- fread(file = "data/starling/filtered_occurrences/native_thinned/32km/starling_native_thinned_merged.csv")
df_natOcc$occurrences <- 1
df_natOcc$x <- round(df_natOcc$x, 5)
df_natOcc$y <- round(df_natOcc$y, 5)
# Merge with env data
df_natAll <- merge.data.table(df_native, df_natOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
# Add range values
df_natAll$range <- "native"
print(unique(df_natAll$occurrences))
fwrite(df_natAll, file = "data/starling/env_var_df/Native_1km_var_with_thinned_occ.csv")

# NZ area -----------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
r_NZ <- terra::rast(paste("data/starling/NZ_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))
## Convert SpatRast to dataframe ------------------------------------#####
df_NZ <- setDT(terra::as.data.frame(r_NZ, na.rm = T, xy = T))
## Read in occurrences ----------------------------------------------#####
df_NZOcc <- fread(file = "data/starling/filtered_occurrences/NZ_thinned/32km/starling_NZ_thinned_merged.csv")
df_NZOcc$occurrences <- 1
# Merge with env data
df_NZAll <- merge.data.table(df_NZ, df_NZOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
# Add range values
df_NZAll$range <- "NZ"
fwrite(df_NZAll, file = "data/starling/env_var_df/NZ_1km_var_with_thinned_occ.csv")

# UK area -----------------------------------------------------------#####
sf_admin <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
sf_UK <- sf_admin[sf_admin$ADMIN %in% c("United Kingdom"),]
## Crop raster to just the UK ---------------------------------------#####
r_UK <- terra::mask(terra::crop(r_native, terra::ext(sf_UK)), sf_UK)
## Convert to dataframe ---------------------------------------------#####
df_UK <- setDT(terra::as.data.frame(r_UK, na.rm = T, xy = T))
df_UK$x <- round(df_UK$x, digits = 5)
df_UK$y <- round(df_UK$y, digits = 5)
## Merge with env data ----------------------------------------------#####
df_UKAll <- merge.data.table(df_UK, df_natOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
df_UKAll$range <- "UK"
## Convert to sf for plotting ---------------------------------------#####
# sf_UKAll <- df_UKAll %>% st_as_sf(coords = c("x", "y"))
# plot(st_geometry(sf_UK))
# plot(st_geometry(sf_UKAll), col = sf_UKAll$occurrences, add = T)
# Merge native and NZ df --------------------------------------------#####
df_combAll <- rbindlist(list(df_natAll, df_NZAll))
print(unique(df_combAll$occurrences))
fwrite(df_combAll, file = "data/starling/env_var_df/NativeNZ_1km_var_with_thinned_occ.csv")
# Merge UK and NZ df ------------------------------------------------#####
df_combNZUK <- rbindlist(list(df_UKAll, df_NZAll))
fwrite(df_combNZUK, file = "data/starling/env_var_df/UKNZ_1km_var_with_thinned_occ.csv")

setNumericRounding(orig_round)
  
