# 02d_myna_clip_future_NZ_area.R
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
ls1 <- list.files("data/future_projections/CHELSA/GFDL-ESM4/ssp126/", pattern = "*.tif$", full.names = T)
ls2 <- list.files("data/future_projections/CHELSA/GFDL-ESM4/ssp370/", pattern = "*.tif$", full.names = T)
ls3 <- list.files("data/future_projections/population_density/", pattern = "*.tif", full.names = T)
varpath <- c(ls1, ls2, ls3, "data/human_var/population_density/BaseYear_1km/pop_dens_2000.tif", "data/tree_cover_pct/gm_ve_v1.tif")

varssp <- c(rep("ssp126", length(ls1)),
            rep("ssp370", length(ls2)),
            c("ssp1", "ssp3"), "", "" )


df_varScaleOffset <- data.frame(fileName = varpath,
                                scale = 0.1,
                                offset = c(rep(c(0, 0, 0, -273.15, -273.15, 0, 0), 2), 0, 0, 0, 0),
                                ssp = varssp)

df_varScaleOffset$scale[14:18] <- 1
df_varScaleOffset$scale[[7]] <- 1



# Use tree_cover extent to redefine all the other extents
tree_cover <- terra::rast("data/tree_cover_pct/gm_ve_v1.tif")
ext(tree_cover) <- c(-180, 180, -90, 90)  # Avoid any rounding issues
tree_cover_cropped <- terra::crop(tree_cover, terra::ext(v_cal))

# Create folder to store outputs 
dir.create("data/myna/future_projections/NZ/", recursive = T, showWarnings = F)
for (i in 1:(dim(df_varScaleOffset)[1])){
  fileName <- df_varScaleOffset$fileName[i]
  sspversion <- df_varScaleOffset$ssp[i]
  r <- terra::rast(fileName)
  rname <- gsub("_2071-2100_gfdl-esm4_ssp126_V.2.1", "", gsub("CHELSA_", "", names(r)))
  rname <- gsub("_2071-2100_gfdl-esm4_ssp370_V.2.1", "", rname)
  names(r) <- rname
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
  if (!(varName %in% c("pop_dens_2090", "tree_cover_pct", "pop_dens_2000"))){
    # Shift coordinates slightly to make sure that cropping and masking
    # yield the same resolution
    dx <- -180 - terra::xmin(r)
    dy <- -90 - terra::ymin(r)
    r <- terra::shift(r, dx = dx, dy = dy)
  }
  r_cropped <- terra::crop(r, terra::ext(v_cal))

  ext(r_cropped) <- ext(tree_cover_cropped)
  # Output cropped/masked raster --------------------------------------#####
  # Replace NA with some zeros as gdd5, gsl has some missing values in 
  # the himalayas which should be zero. Same goes for urb_frac_2000 where
  # there are some missing values in potentially extremely rural areas
  # or wetlands
  if (varName %in% c("gdd5", "gsl", "pop_dens_2090", "pop_dens_2000")){
    r_cropped <- terra::rast(raster::reclassify(raster::raster(r_cropped), cbind(NA, 0)))
  } 
  r_cropMask <- terra::mask(r_cropped, v_cal)
  print(ext(r_cropMask))
  terra::writeRaster(r_cropMask, paste("data/myna/future_projections/NZ/", varName, "_cropped_masked_", sspversion, ".tif", sep = ""), overwrite = T)
  # Quick plot of all cropped/masked layers ---------------------------#####
  outputPNG <- paste("data/myna/future_projections/NZ/", varName, "_cropped_masked_", sspversion, ".png", sep = "")
  png(filename = outputPNG, width = 11.7, height = 8.3, units = "in", res = 300)
  plot(r_cropMask, colNA = "grey", ext = ext(v_cal))
  # Add shorelines
  plot(v_cal, add = T)
  title(main = varName)
  dev.off()
}

