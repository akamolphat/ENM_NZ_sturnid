# 01_make_nativeNZ_dataset_block.R -----------------------------------------#####
# This script creates the dataset for performing the test ENM
# Saves the file as .Rdata to be re-called future runs
#
# Libraries ---------------------------------------------------------
library(sf)
library(raster)
library(terra)
library(tidyverse)
library(data.table)
library(biomod2)
library(blockCV)

# Define input ------------------------------------------------------
range <- "nativeNZ"
ls_varnames <- c("bio12", 
                 "gsl",
                 "gdd5",
                 "pop_dens_2000",
                 "tree_cover_pct")

biomodDataRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodData.Rdata", sep = "")
SAC_output <- paste("results/myna/BIOMOD/", range, "/", range, "SAC_speciesData.pdf", sep = "")
sbRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")

# Create folders for input ------------------------------------------
dir.create(paste("data/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)

# Read in rasters ---------------------------------------------------
varpath <- c(paste("data/myna/comb_calibration_area/", ls_varnames, "_cropped_masked.tif", sep = ""))

r_merged <- terra::rast(varpath)

# Clean up rasters --------------------------------------------------
# Assign variable names
names(r_merged) <- ls_varnames
# Rename pop_dens_2000 to pop_dens
names(r_merged)[names(r_merged) == "pop_dens_2000"] <- "pop_dens"
# Convert SpatRaster to RasterStack. Latest version of biomod2 
# supports SpatRaster but not the version on NeSI (3.5.1)
r_merged <- raster::stack(r_merged)

# Mask NA
r_merged <- raster::mask(r_merged, calc(r_merged, fun = sum))  # Create a mask to remove any cells with NA

# Make sure it is a RasterStack object
r_merged <- raster::stack(r_merged)

# Read shapefiles for crs -------------------------------------------
sf_cal <- st_read(dsn = path.expand("data/myna/native_calibration_area/native_calibration_area.shp"))
# Read occurrences --------------------------------------------------
occfilepath1 <- paste("data/myna/filtered_occurrences/NZ_thinned/16km/myna_NZ_thinned_merged.csv", sep = "")
occfilepath2 <- paste("data/myna/filtered_occurrences/native_thinned/16km/myna_native_thinned_merged.csv", sep = "")

dt_sp1 <- fread(occfilepath1)
dt_sp2 <- fread(occfilepath2)
dt_sp <- rbind(dt_sp1, dt_sp2)
dt_sp <- dt_sp %>% drop_na()

# Remove any cells with NA values -----------------------------------
# 
# This is done here instead of earlier when clipping as some 
# different variables have different NA cells
#
# Extract values at occurrence points
dt_sp <- data.frame(dt_sp, raster::extract(r_merged, dt_sp[,c("x", "y")]))
dt_sp <- dt_sp %>% drop_na()
# convert occurrences dataframe to sf object and coordinate system from raster
sf_sp <- st_as_sf(dt_sp, coords = c("x", "y"), crs = crs(sf_cal))


# Create plots for effective range of spatial autocorrelation -------#####
# The higher sample number is likely more accurate but also takes more
# time
sac_sp <- spatialAutoRange(rasterLayer = r_merged,
                           speciesData = sf_sp,  # At species occurrence points
                           doParallel = TRUE,
                           showPlots = F)

sac_sp2 <- spatialAutoRange(rasterLayer = r_merged,
                            sampleNumber = 10000,  # 10000 points across landscape
                            doParallel = TRUE,
                            showPlots = F)

pdf("results/myna/BIOMOD/nativeNZ/sac_sp_all.pdf")
sac_sp$plots$barchart
sac_sp2$plots$barchart
dev.off()
save(sac_sp, sac_sp2, file = "data/myna/BIOMOD/nativeNZ/sac_sp.Rdata")

# Create input for BIOMOD2 ------------------------------------------
## Generate random background points --------------------------------
##
## As random background points are being generated, the input 
## environmental variables must a raster or spatRaster 
##
myRespXY <- st_coordinates(sf_sp)
myResp <- as.numeric(as.factor(sf_sp$species))
# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = r_merged, # explanatory raster data
                                     resp.xy = myRespXY,
                                     resp.name = "myna",
                                     PA.nb.rep = 2,
                                     PA.nb.absences	= 100000,
                                     na.rm = T)

save(myBiomodData, file = biomodDataRdata)

load(file = biomodDataRdata)
# Extract points from myBiomodData ----------------------------------#####
# This is to perform blockCV
# Define function to get data from myBiomodData
get_PAtab <- function(bfd){
  # Function from: http://rstudio-pubs-static.s3.amazonaws.com/416446_3ef37751ae1e4e569964dabc09a75b56.html
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA # Only if there are PA.nb.rep
  )
}
# Extract points from all pseudoabsences replicates -----------------#####
dt_spAll <- get_PAtab(myBiomodData) %>%
  # dplyr::select(status, x, y) %>%
  rename(species = status)
sf_spAll <- dt_spAll %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_cal)) # convert to sf
# 1.b. Formatting data for dataset without gdd5
myRespXY <- dt_spAll[,c("x", "y")]
myResp <- as.numeric(as.factor(dt_spAll$species))
PATab <- as.matrix(dt_spAll[,c("PA1", "PA2")])

# Create spatial blocks ---------------------------------------------#####
sb <- spatialBlock(speciesData = sf_spAll,
                    species = "species",
                    rasterLayer = r_merged,
                    theRange = 2000000, # size of the blocks
                    k = 5,
                    selection = "random",
                    iteration = 100, # find evenly dispersed folds
                    biomod2Format = TRUE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)

save(sb, file = sbRdata)
pdf(SAC_output, width = 11.7, height = 8.3)
sb$plots #+ geom_sf(data = sf_sp, alpha = 0.5, aes(col = factor(species)))
dev.off()
