# NZ_create_dataset_block.R -----------------------------------------#####
# This script creates the dataset for performing the test ENM
# Saves the file as .Rdata to be re-called future runs
#
# Load libraries ----------------------------------------------------#####
library(blockCV)
library(raster)
library(sf)
library(biomod2)
library(dismo)
library(tidyverse)
library(data.table)
# Define run name and models ----------------------------------------#####
range <- "NZ"
ls_varnames <- c(
                 "bio12", 
                 "gsl", 
                 "gdd5",
                 "pop_dens_2000",
                 "tree_cover_pct")
dir.create(paste("data/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)

# Define .Rdata output ----------------------------------------------#####
sbRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
biomodDataRdatav1 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav1.Rdata", sep = "")
SAC_output <- paste("results/myna/BIOMOD/", range, "/", range, "SAC_speciesData.pdf", sep = "")

# Read shapefile ----------------------------------------------------#####
shpfilepath <- paste("data/myna/", range, "_calibration_area/", range, "_calibration_area.shp", sep = "")
sf_cal <- st_read(dsn = path.expand(shpfilepath))
# Read occurrences --------------------------------------------------#####
occfilepath <- paste("data/myna/filtered_occurrences/", range, "_thinned/16km/myna_", range, "_thinned_merged.csv", sep = "")
dt_sp <- fread(occfilepath)
dt_sp <- dt_sp %>% drop_na()

# Read tiff files ---------------------------------------------------#####
varpath <- c(paste("data/myna/", range, "_calibration_area/", ls_varnames, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
# Assign variable names
names(r_var) <- ls_varnames
# Rename pop_dens_2000 to pop_dens
names(r_var)[7] <- "pop_dens"
# Mask NA
r_var <- mask(r_var, calc(r_var,fun = sum))  # Create a mask to remove any cells with NA
r_var <- stack(r_var)
# Remove any cells with NA values. This is done here instead of earlier 
# when clipping as some different variables have different NA cells
# Extract values at occurrence points
dt_sp <- data.frame(dt_sp, raster::extract(r_var, dt_sp[,c("x", "y")]))
dt_sp <- dt_sp %>% drop_na()
# convert occurrences dataframe to sf object and coordinate system from raster
sf_sp <- st_as_sf(dt_sp, coords = c("x", "y"), crs = crs(sf_cal))
# Remove all points outside of calibration area to make sure no points fall 
# outside shapefile
sf::sf_use_s2(FALSE)
sf_sp <- st_filter(sf_sp, sf_cal) 
# Create plots for effective range of spatial autocorrelation -------#####
# The higher sample number is likely more accurate but also takes more
# time
sac_sp <- spatialAutoRange(rasterLayer = r_var,
                           speciesData = sf_sp,  # At species occurrence points
                           doParallel = TRUE,
                           showPlots = F)

sac_sp2 <- spatialAutoRange(rasterLayer = r_var,
                           sampleNumber = 10000,  # 10000 points across landscape
                           doParallel = TRUE,
                           showPlots = F)

pdf("results/myna/BIOMOD/NZ/sac_sp.pdf")
sac_sp$plots$barchart
sac_sp2$plots$barchart
dev.off()
save(sac_sp, sac_sp2, file = "data/myna/BIOMOD/NZ/sac_sp.Rdata")

# BIOMOD2 -----------------------------------------------------------#####
# Generate random background points ---------------------------------#####
myRespXY <- st_coordinates(sf_sp)
myResp <- as.numeric(as.factor(sf_sp$species))
# 1. Formatting Data
# Without bio1
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = r_var, # explanatory raster data
                                     resp.xy = myRespXY,
                                     resp.name = "myna",
                                     PA.nb.rep = 2,
                                     PA.nb.absences	= 50000,
                                     na.rm = T)

save(myBiomodData, file = biomodDataRdatav1)
# load(file = biomodDataRdatav1)
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

save(sb, file = sbRdata)
pdf(SAC_output, width = 11.7, height = 8.3)
sb$plots #+ geom_sf(data = sf_sp, alpha = 0.5, aes(col = factor(species)))
dev.off()
