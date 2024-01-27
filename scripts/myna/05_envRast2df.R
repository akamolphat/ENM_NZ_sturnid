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
ls_var <- c(paste("bio", seq(1, 19), sep = ""),                          # CHELSA bioclim var
            "gsl", "gst", "gsp", "gdd5",                                 # growing season var
            "tree_cover_pct",
            "urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled", "log_pop_dens")  # human var
# Native area -------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
r_native <- terra::rast(paste("data/myna/native_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))

## Convert SpatRast to dataframe ------------------------------------#####
df_native <- setDT(terra::as.data.frame(r_native, na.rm = T, xy = T))
df_native$x <- round(df_native$x, 5)
df_native$y <- round(df_native$y, 5)
# Read in occurrences -----------------------------------------------#####
df_natOcc <- fread(file = "data/myna/filtered_occurrences/native_thinned/16km/myna_native_thinned_merged.csv")
df_natOcc$occurrences <- 1
df_natOcc$x <- round(df_natOcc$x, 5)
df_natOcc$y <- round(df_natOcc$y, 5)
# Merge with env data
df_natAll <- merge.data.table(df_native, df_natOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
# Add range values
df_natAll$range <- "native"
print(unique(df_natAll$occurrences))
fwrite(df_natAll, file = "data/myna/env_var_df/Native_1km_var_with_thinned_occ.csv")

# NZ area -----------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
r_NZ <- terra::rast(paste("data/myna/NZ_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))
## Convert SpatRast to dataframe ------------------------------------#####
df_NZ <- setDT(terra::as.data.frame(r_NZ, na.rm = T, xy = T))
## Read in occurrences ----------------------------------------------#####
df_NZOcc <- fread(file = "data/myna/filtered_occurrences/NZ_thinned/16km/myna_NZ_thinned_merged.csv")
df_NZOcc$occurrences <- 1
# Merge with env data
df_NZAll <- merge.data.table(df_NZ, df_NZOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
# Add range values
df_NZAll$range <- "NZ"
fwrite(df_NZAll, file = "data/myna/env_var_df/NZ_1km_var_with_thinned_occ.csv")
# Merge native and NZ df --------------------------------------------#####
df_combAll <- rbindlist(list(df_natAll, df_NZAll))
print(unique(df_combAll$occurrences))
fwrite(df_combAll, file = "data/myna/env_var_df/NativeNZ_1km_var_with_thinned_occ.csv")

setNumericRounding(orig_round)
