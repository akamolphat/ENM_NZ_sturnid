# 02b_myna_clip_env_NZ_area.R
#
# This script clips the environmental variables to the NZ area.
# The NZ calibration area is the same as the NZ plotting area.
#
# This script is very similar to 02_myna_clip_env_calibration_area.R
# except that this is on the NZ dataset
#
# Instead of using raster::stack or brick, we are just going to loop
# through each layer instead, in hopes that it will avoid the memory
# issues
#
# Load libraries ----------------------------------------------------#####
library(rasterVis)
library(raster)
library(terra)  # terra is designed to replace raster and it just seems to be faster and more efficient
library(ggplot2)
library(data.table)
library(tidyverse)
library(sf)
# library(rgdal)
# Read in shapefiles ------------------------------------------------#####
v_cal <- terra::vect(path.expand("data/myna/NZ_calibration_area/NZ_calibration_area.shp"))

# Read in tiff file -------------------------------------------------#####
# Urban land extent has a smaller extent than CHELSA and therefore
# will be cropped separately
varpath <- c(paste("data/CHELSA/CHELSA_bio", seq(1,19), "_1981-2010_V.2.1.tif", sep = ""),
             paste("data/CHELSA/CHELSA_", c("gsl", "gst", "gsp", "gdd5"), "_1981-2010_V.2.1.tif", sep = ""),
             "data/human_var/urban_land_extent/baseyear-geotiff/urb_frac_2000.tif",
             "data/human_var/population_density/BaseYear_1km/pop_dens_2000.tif",
             "data/human_var/impervious_surface_area/ngdc_isa_gcs_resampled.tif",
             "data/tree_cover_pct/gm_ve_v1.tif",
             "data/CHELSA/dem.tif")

df_varScaleOffset <- data.frame(fileName = varpath,
                                scale = 0.1,
                                offset = c(-273.15, 0, 0, 0, -273.15, -273.15, 0,
                                           -273.15, -273.15, -273.15, -273.15, 0,
                                           0, 0, 0, 0, 0, 0, 0, 0, -273.15, 0, 0, 
                                           0, 0, 0, 0, 0 
                                ))

df_varScaleOffset$scale[[20]] <- 1
df_varScaleOffset$scale[24:28] <- 1

# snap_extent <- extent(c(-180, 180, -55.875, 83.75833))  # urb_frac_2000 extent
# snap_extent <- extent(c(-180, 180, -55.775, 83.625))  # just short of pop_tot extent
# snap_extent <- terra::ext(c(-180, 180, -55.775, 75))  # extent of pop_dens and isa product
# The snap_extent is to make sure that the rasters are well aligned.

# Use tree_cover extent to redefine all the other extents
tree_cover <- terra::rast("data/tree_cover_pct/gm_ve_v1.tif")
ext(tree_cover) <- c(-180, 180, -90, 90)  # Avoid any rounding issues
tree_cover_cropped <- terra::crop(tree_cover, terra::ext(v_cal))

for (i in 1:(dim(df_varScaleOffset)[1])){
  fileName <- df_varScaleOffset$fileName[i]
  r <- terra::rast(fileName)
  names(r) <- gsub("_1981.2010_V.2.1", "", gsub("CHELSA_", "", names(r)))
  if (names(r) == "gm_ve_v1"){
    names(r) <- "tree_cover_pct"
  }
  varName <- names(r)
  print(varName)
  if (varName == "gsp"){
    # Had to convert back to raster first as terra::classify seems to cause
    # memory issues on the cluster
    r <- terra::rast(raster::reclassify(raster::raster(r), cbind(4e7, Inf, 0), right = F))
  } else if (varName == "tree_cover_pct"){
    r <- terra::rast(raster::reclassify(raster::raster(r), cbind(200, 300, NA), right = F))
  }
  scale <- df_varScaleOffset$scale[[i]]
  offset <- df_varScaleOffset$offset[[i]]
  # gain(r) <- scale
  terra::scoff(r) <- matrix(c(scale, offset), nrow = 1)
  
  # Crop/mask native range --------------------------------------------#####
  if (!(varName %in% c("urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled", "tree_cover_pct"))){
    # Shift coordinates slightly to make sure that cropping and masking
    # yield the same resolution
    dx <- -180 - terra::xmin(r)
    dy <- -90 - terra::ymin(r)
    r <- terra::shift(r, dx = dx, dy = dy)
  }
  r_cropped <- terra::crop(r, terra::ext(v_cal))

  ext(r_cropped) <- ext(tree_cover_cropped)
  # Output cropped/masked raster --------------------------------------#####
  # Replace NA with some zeros as gdd5, gsl, and gsp has some missing values in 
  # the himalayas which should be zero. Same goes for urb_frac_2000 where
  # there are some missing values in potentially extremely rural areas
  # or wetlands
  if (varName %in% c("gsp", "gdd5", "gsl", "urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled")){
    r_cropped <- terra::rast(raster::reclassify(raster::raster(r_cropped), cbind(NA, 0)))
  } 
  r_cropMask <- terra::mask(r_cropped, v_cal)
  print(ext(r_cropMask))
  terra::writeRaster(r_cropMask, paste("data/myna/NZ_calibration_area/", names(r_cropMask), "_cropped_masked.tif", sep = ""), overwrite = T)
  # Quick plot of all cropped/masked layers ---------------------------#####
  outputPNG <- paste("data/myna/NZ_calibration_area/", varName, "_cropped_masked.png", sep = "")
  png(filename = outputPNG, width = 11.7, height = 8.3, units = "in", res = 300)
  plot(r_cropMask, colNA = "grey", ext = ext(v_cal))
  # Add shorelines
  plot(v_cal, add = T)
  title(main = varName)
  dev.off()
  if (varName == "pop_dens_2000"){
    log_pop_dens <- log(r_cropMask + 1)
    names(log_pop_dens) <- "log_pop_dens"
    terra::writeRaster(log_pop_dens, paste("data/myna/NZ_calibration_area/", names(log_pop_dens), "_cropped_masked.tif", sep = ""), overwrite = T)
    outputPNG <- paste("data/myna/NZ_calibration_area/", names(log_pop_dens), "_cropped_masked.png", sep = "")
    png(filename = outputPNG, width = 11.7, height = 8.3, units = "in", res = 300)
    plot(log_pop_dens, colNA = "grey", ext = ext(v_cal))
    # Add shorelines
    plot(v_plot, add = T)
    title(main = "log_pop_dens")
    dev.off()
  }
}

