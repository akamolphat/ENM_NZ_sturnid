# This script creates a dataframe with subsampled gridcells from the 
# calibration area. Grid cells with presence record is marked as 1.
#
# Due to the extremely large number of points for the starling dataset,
# only a subsample of points were taken from the native and the NZ
# calibration range area
#
# This is also to reduce the number of points used for the performing 
# correlations.
#
# For the native range, the number of points subsampled is 100,000 points
# and for the NZ range, the number of points subsampled is 100,000 points
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
            "urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled", "log_pop_dens", # human var
            "dem")  
# Native area -------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
print("Reading in native raster files")
r_native <- terra::rast(paste("data/starling/native_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))

## Convert SpatRast to dataframe ------------------------------------#####
# df_native <- setDT(terra::as.data.frame(r_native, na.rm = T, xy = T))
# Remove r_native object for RAM reasons
# rm(r_native)
# gc()
print("Subsample points from native raster.")
df_nativesub <- spatSample(x = r_native, size = 100000, method="random", replace=F, na.rm=T, xy = T,
                       as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE)
# df_native$x <- round(df_native$x, 5)
# df_native$y <- round(df_native$y, 5)
# Read in occurrences -----------------------------------------------#####
print("Read in native occurrence points")
df_natOcc <- fread(file = "data/starling/filtered_occurrences/native_thinned/32km/starling_native_thinned_merged.csv")
print("Extract values at native occurrence points")
df_natOcc <- terra::extract(x = r_native, y = df_natOcc[,c("x", "y")], xy = T, ID = F)
df_natOcc <- df_natOcc[,colnames(df_nativesub)]
df_nativesub$occurrences <- NA
df_natOcc$occurrences <- 1
# df_natOcc$x <- round(df_natOcc$x, 5)
# df_natOcc$y <- round(df_natOcc$y, 5)
# Merge with env data
# df_natAll <- merge.data.table(df_native, df_natOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
df_natAll <- rbind(df_nativesub, df_natOcc)
# Add range values
df_natAll$range <- "native"
print(unique(df_natAll$occurrences))
print("Output native dataframe with occurrences and subsampled background points")
fwrite(df_natAll, file = "data/starling/env_var_df/Native_1km_varSubsampled_with_thinned_occ.csv")
rm(r_native, df_natOcc, df_native)
gc()
# df_natAll <- fread("data/starling/env_var_df/Native_1km_varSubsampled_with_thinned_occ.csv")
# NZ area -----------------------------------------------------------#####
## Read in tiff file ------------------------------------------------#####
print("Read in NZ raster")
r_NZ <- terra::rast(paste("data/starling/NZ_calibration_area/", ls_var, "_cropped_masked.tif", sep = ""))
## Convert SpatRast to dataframe ------------------------------------#####
# df_NZ <- setDT(terra::as.data.frame(r_NZ, na.rm = T, xy = T))
# rm(r_NZ)
print("Subsample points from NZ raster")
df_NZsub <- spatSample(x = r_NZ, size = 100000, method="random", replace=F, na.rm=T, xy = T,
                       as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE)
## Read in occurrences ----------------------------------------------#####
print("Read in NZ occurrences")
df_NZOcc <- fread(file = "data/starling/filtered_occurrences/NZ_thinned/32km/starling_NZ_thinned_merged.csv")
print("Extract values at NZ occurrences")
df_NZOcc <- terra::extract(x = r_NZ, y = df_NZOcc[,c("x", "y")], xy = T, ID = F)
df_NZOcc <- df_NZOcc[,colnames(df_NZsub)]
# Add occurrences ---------------------------------------------------
df_NZsub$occurrences <- NA
df_NZOcc$occurrences <- 1
# Merge with env data
# df_NZAll <- merge.data.table(df_NZ, df_NZOcc[,c("x", "y", "occurrences")], by = c("x", "y"), all.x = T)
df_NZAll <- rbind(df_NZsub, df_NZOcc)
# Add range values
df_NZAll$range <- "NZ"
print("Output NZ dataframe with occurrences and subsampled background points")
fwrite(df_NZAll, file = "data/starling/env_var_df/NZ_1km_varSubsampled_with_thinned_occ.csv")
# Merge native and NZ df --------------------------------------------#####
df_combAll <- rbindlist(list(df_natAll, df_NZAll))
print(unique(df_combAll$occurrences))
print("Output combined NZ and native dataframe")
fwrite(df_combAll, file = "data/starling/env_var_df/NativeNZ_1km_varSubsampled_with_thinned_occ.csv")

setNumericRounding(orig_round)


  
