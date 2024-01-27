# native_starling_create_dataset_block.R --------------------------------
#
# This script creates the dataset for performing the test ENM
# Saves the file as .Rdata to be re-called future runs
#
# In the previous 
# Load libraries ----------------------------------------------------
library(blockCV)
library(raster)
library(sf)
library(biomod2)
library(dismo)
library(tidyverse)
library(data.table)
# Define run name and models ----------------------------------------
range <- "native"
ls_varnames <- c("bio3",  # Not correlated to other variables, important in Shivambu et al. 2019, no proper interpretation (isothermality)
                 "bio5",  # Use to capture the heat limits during breeding season, likely captured in GDD5
                 "bio12", 
                 "gsl",   
                 "bio15", # Not correlated to other variables, no proper biological interpretation
                 "gdd5", 
                 "pop_dens_2000",
                 "tree_cover_pct",
                 "dem")
dir.create(paste("data/starling/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/starling/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)

# Define .Rdata output ----------------------------------------------#####
sbRdata <- paste("data/starling/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
sbRdataALL <- paste("data/starling/BIOMOD/", range, "/", range, "_sb_blocking_all.Rdata", sep = "")

biomodDataRdata <- paste("data/starling/BIOMOD/", range, "/", range, "_myBiomodData.Rdata", sep = "")

SAC_output <- paste("results/starling/BIOMOD/", range, "/", range, "SAC_speciesData.pdf", sep = "")

# Read shapefile ----------------------------------------------------#####
shpfilepath <- paste("data/starling/", range, "_calibration_area/starling_", range, "_calibration_area.shp", sep = "")
sf_cal <- st_read(dsn = path.expand(shpfilepath))
# Read occurrences --------------------------------------------------#####
occfilepath <- paste("data/starling/filtered_occurrences/", range, "_thinned/32km/starling_", range, "_thinned_merged.csv", sep = "")
dt_sp <- fread(occfilepath)
dt_sp <- dt_sp %>% drop_na()

# Read tiff files ---------------------------------------------------#####
varpath <- c(paste("data/starling/", range, "_calibration_area/", ls_varnames, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
# Assign variable names
names(r_var) <- ls_varnames
# Rename pop_dens_2000 to pop_dens
names(r_var)[names(r_var) == "pop_dens_2000"] <- "pop_dens"
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
# This is based on 10000 random points across the landscape which might
# be better to use as there might be strong spatial autocorrelations at
# occurrence sites anyways.
sac_sp2 <- spatialAutoRange(rasterLayer = r_var,
                            sampleNumber = 10000,  # 10000 points across landscape
                            doParallel = TRUE,
                            showPlots = F)

pdf("results/starling/BIOMOD/native/sac_sp_all.pdf")
sac_sp$plots$barchart
sac_sp2$plots$barchart
dev.off()
save(sac_sp, sac_sp2, file = "data/starling/BIOMOD/native/sac_sp.Rdata")
# load(file = "data/starling/BIOMOD/native/sac_sp.Rdata")

# BIOMOD2 -----------------------------------------------------------#####
# Generate random background points ---------------------------------#####
myRespXY <- st_coordinates(sf_sp)
myResp <- as.numeric(as.factor(sf_sp$species))
# 1. Formatting Data
# Without bio1
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = r_var, # explanatory raster data
                                     resp.xy = myRespXY,
                                     resp.name = "starling",
                                     PA.nb.rep = 2,
                                     PA.nb.absences	= 50000,
                                     na.rm = T)

save(myBiomodData, file = biomodDataRdata)
# load(file = biomodDataRdata)
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
                   rasterLayer = r_var,
                   theRange = 4000000, # size of the blocks
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
