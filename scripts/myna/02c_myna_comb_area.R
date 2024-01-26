# Merge NZ and native dataset ---------------------------------------
library(terra)
library(sf)

# 1. ----------------------------------------------------------------
sf_nat <- st_read(dsn = path.expand("data/myna/native_calibration_area/native_calibration_area.shp"))
sf_NZ <- st_read(dsn = path.expand("data/myna/NZ_calibration_area/NZ_calibration_area.shp"))
# 2. ----------------------------------------------------------------
sf_comb <- st_union(sf_NZ, sf_nat)
dir.create("data/myna/comb_calibration_area", recursive = T, showWarnings = F)
st_write(sf_comb, "data/myna/comb_calibration_area/comb_calibration_area.shp", delete_dsn = T)

var_ls <- c(paste("bio", seq(1, 19), sep = ""), 
            c("gsl", "gst", "gsp", "gdd5"), 
            c("urb_frac_2000", "pop_dens_2000", "ngdc_isa_gcs_resampled", "tree_cover_pct"))
sf_comb <- st_read("data/myna/comb_calibration_area/comb_calibration_area.shp")

for (i in seq(1, length(var_ls))){
  varName <- var_ls[i]
  NZvarpath <- paste("data/myna/NZ_calibration_area/", varName, "_cropped_masked.tif", sep = "")
  Nativevarpath <- paste("data/myna/native_calibration_area/", varName, "_cropped_masked.tif", sep = "")
  Outpath <- paste("data/myna/comb_calibration_area/", varName, "_cropped_masked.tif", sep = "")
  r_NZ <- terra::rast(NZvarpath)
  r_native <- terra::rast(Nativevarpath)
  r_comb <- terra::merge(r_NZ, r_native)
  terra::writeRaster(r_comb, Outpath, overwrite = T)
  outputPNG <- paste("data/myna/comb_calibration_area/", varName, "_cropped_masked.png", sep = "")
  png(filename = outputPNG, width = 11.7, height = 8.3, units = "in", res = 300)
  terra::plot(r_comb, colNA = "grey", ext = ext(sf_comb))
  # Add shorelines
  plot(st_geometry(sf_comb), add = T)
  title(main = varName)
  dev.off()
}

