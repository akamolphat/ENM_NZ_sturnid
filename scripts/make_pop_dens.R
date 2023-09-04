# This script converts population per gridcell to population per km2
# library(raster) 
library(terra) # Note that the terra package was used instead as this preserved the name of the layer
# BaseYr ------------------------------------------------------------
fileName <- "data/human_var/population_per_gridcell/BaseYear_1km/baseYr_total_2000.tif"
# Reading raster
print("Reading raster")
r <- terra::rast(fileName)
# Calculate area
print("Calculate area")
r_area <- terra::cellSize(r, mask = F, unit = "km")
# Calculate density
print("Calculate density")
pop_dens <- r/r_area
names(pop_dens) <- "pop_dens_2000"
print("Create folder")
dir.create("data/human_var/population_density/BaseYear_1km", recursive = T, showWarnings = F)
print("Output file")
terra::writeRaster(pop_dens, "data/human_var/population_density/BaseYear_1km/pop_dens_2000.tif", overwrite = T)

# SSP1 --------------------------------------------------------------
fileName <- "data/future_projections/population_per_gridcell/SSP1_1km/ssp1_total_2090.nc4"
# Reading raster
print("Reading SSP1 raster")
r <- terra::rast(fileName)
# Calculate area
print("Calculate area")
r_area <- terra::cellSize(r, mask = F, unit = "km")
# Calculate density
print("Calculate density; reusing r_area from previous raster")
pop_dens <- r/r_area
names(pop_dens) <- "pop_dens_2090"
print("Create folder")
dir.create("data/future_projections/population_density/", recursive = T, showWarnings = F)
print("Output pop_dens SSP1 file")
terra::writeRaster(pop_dens, "data/future_projections/population_density/pop_dens_2090_ssp1.tif", overwrite = T)
# SSP3 --------------------------------------------------------------
fileName <- "data/future_projections/population_per_gridcell/SSP3_1km/ssp3_total_2090.nc4"
# Reading raster
print("Reading SSP3 raster")
r <- terra::rast(fileName)
# Calculate density
print("Calculate density; reusing r_area from previous raster")
pop_dens <- r/r_area
names(pop_dens) <- "pop_dens_2090"
print("Output pop_dens SSP3 file")
terra::writeRaster(pop_dens, "data/future_projections/population_density/pop_dens_2090_ssp3.tif", overwrite = T)