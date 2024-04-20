# native_create_dataset_block.R -----------------------------------------#####
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
range <- "native"
ls_varnames <- c("bio1",  # This variable is highly correlated to gdd5, but is included to test out both variables
                 "bio5", "bio6",  # Use to capture the extremes but no biological explanation, likely captured in bio1 or GDD5
                 "bio12", 
                 "gsl", 
                 # "bio10", 
                 "gdd5",  
                 # "gsp",  # GSP has strong correlation with bio12 (annual ppt) and might not be as important as plants may fruit during periods with no ppt
                 # "urb_frac_2000",
                 "pop_dens_2000",
                 # "log_pop_dens",  # log of population density instead 
                 # "ngdc_isa_gcs_resampled",
                 "tree_cover_pct")
dir.create(paste("data/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)

# Define .Rdata output ----------------------------------------------#####
sbRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
sbRdataALL <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking_all.Rdata", sep = "")

biomodDataRdatav1 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav1.Rdata", sep = "")
biomodDataRdatav2 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav2.Rdata", sep = "")
biomodDataRdatav3 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav3.Rdata", sep = "")
biomodDataRdatav4 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav4.Rdata", sep = "")
biomodDataRdatav5 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav5.Rdata", sep = "")
biomodDataRdatav6 <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodDatav6.Rdata", sep = "")

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

pdf("results/myna/BIOMOD/native/sac_sp_all.pdf")
sac_sp$plots$barchart
sac_sp2$plots$barchart
dev.off()
save(sac_sp, sac_sp2, file = "data/myna/BIOMOD/native/sac_sp.Rdata")
# load(file = "data/myna/BIOMOD/native/sac_sp.Rdata")

# BIOMOD2 -----------------------------------------------------------#####
# Generate random background points ---------------------------------#####
myRespXY <- st_coordinates(sf_sp)
myResp <- as.numeric(as.factor(sf_sp$species))
# 1. Formatting Data
# Without bio1, bio5, and bio6
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = dropLayer(r_var, c(1,2,3)), # explanatory raster data
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
# 1. Formatting Data
# Version 2 ---------------------------------------------------------#####
# Without gdd5, bio5, and bio6
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = dropLayer(r_var, c(2,3,6)), # explanatory raster data
                                       resp.xy = myRespXY,
                                       resp.name = "myna",
                                       PA.table = PATab,
                                       na.rm = T)

save(myBiomodData, file = biomodDataRdatav2)
# load(file = biomodDataRdatav2)
# Version 3 ---------------------------------------------------------#####
# With bio5 and bio6 instead of bio1/gdd5
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = dropLayer(r_var, c(1,6)), # explanatory raster data
                                       resp.xy = myRespXY,
                                       resp.name = "myna",
                                       PA.table = PATab,
                                       na.rm = T)

save(myBiomodData, file = biomodDataRdatav3)
# Version 4 ---------------------------------------------------------#####
# With bio5 and bio6 in addition to gdd5
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = dropLayer(r_var, 1), # explanatory raster data
                                       resp.xy = myRespXY,
                                       resp.name = "myna",
                                       PA.table = PATab,
                                       na.rm = T)

save(myBiomodData, file = biomodDataRdatav4)
# Version 5 ---------------------------------------------------------#####
# With only bio6 in addition to gdd5
myBiomodData <- BIOMOD_FormatingData(  resp.var = myResp,
                                       expl.var = dropLayer(r_var, c(1,2)), # explanatory raster data
                                       resp.xy = myRespXY,
                                       resp.name = "myna",
                                       PA.table = PATab,
                                       na.rm = T)

save(myBiomodData, file = biomodDataRdatav5)
# load(file = biomodDataRdatav5)
# Version 6 ---------------------------------------------------------#####
# With bio6 instead of gdd5
myBiomodData <- BIOMOD_FormatingData(  resp.var = myResp,
                                       expl.var = dropLayer(r_var, c(1,2,6)), # explanatory raster data
                                       resp.xy = myRespXY,
                                       resp.name = "myna",
                                       PA.table = PATab,
                                       na.rm = T)

save(myBiomodData, file = biomodDataRdatav6)
# load(file = biomodDataRdatav6)
# Create spatial blocks ---------------------------------------------#####
sb <- spatialBlock(speciesData = sf_spAll,
                   species = "species",
                   rasterLayer = r_var,
                   theRange = 1000000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

sb2 <- spatialBlock(speciesData = sf_spAll,
                   species = "species",
                   rasterLayer = r_var,
                   theRange = 2000000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

sb3 <- spatialBlock(speciesData = sf_spAll,
                    species = "species",
                    rasterLayer = r_var,
                    theRange = 3000000, # size of the blocks
                    k = 5,
                    selection = "random",
                    iteration = 100, # find evenly dispersed folds
                    biomod2Format = TRUE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)
save(sb, sb2, sb3, file = sbRdataALL)
sb <- sb2
save(sb, file = sbRdata)
pdf(SAC_output, width = 11.7, height = 8.3)
sb$plots #+ geom_sf(data = sf_sp, alpha = 0.5, aes(col = factor(species)))
dev.off()
