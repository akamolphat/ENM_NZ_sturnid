# plot_myna_cross_projections.R ---------------------------------
# 
# This script plots projections and cross-projections for the myna data
#
# Library and source scripts ----------------------------------------
source("scripts/patternfun.R")
library(terra)
library(data.table)
library(sf)
library(biomod2)
library(raster)
# Define functions --------------------------------------------------
sdm_threshold <- function(sdm, occs, percentile = 0, binary = FALSE){
  # Function taken from Cecina Babich Morrow's github.io page
  #
  # https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/
  #
  # Function was modified to be able to define own 
  # percentile training presence threshold
  #
  occPredVals <- raster::extract(sdm, occs)
  if (percentile > 100 | percentile < 0){
    stop("percentile is not within range (0 - 100)")
  } else if (percentile == 0) {
    thresh <- min(na.omit(occPredVals))
  } else {
    pratio <- 1 - percentile/100
    if (length(occPredVals) < (100/percentile)){
      pthres <- floor(length(occPredVals) * pratio)
    } else {
      pthres <- ceiling(length(occPredVals) * pratio)
    }
    thresh <- rev(sort(occPredVals))[pthres]
  }
  sdm_thresh <- sdm
  if (binary) {
    # sdm_thresh[sdm_thresh >= thresh] <- 1
    values(sdm_thresh)  <- as.numeric(values(sdm_thresh) >= thresh)
  } else {
    sdm_thresh[sdm_thresh < thresh] <- NA
  }
  print(thresh)
  return(list(threshold = thresh, rast = sdm_thresh))
}

rast_binary_thres <- function(rast, threshold){
  r <- rast
  values(r) <- as.numeric(values(r) >= threshold)
  return(r)
}
# Define some input variable ----------------------------------------
sp <- "myna"
NZrunName <- "NZ_fullBIOMODv1"
nativerunName <- "native_fullBIOMODv1a"
nativeNZrunName <- "nativeNZ_fullBIOMODv1a"
mod <- "MAXENT.Phillips"
spfoldername <- "A_tristis_distribution_map"

# Read in shape files -----------------------------------------------
sf_NZ <- st_read("data/myna/NZ_calibration_area/NZ_calibration_area.shp")
sf_nat <- st_read("data/myna/native_calibration_area/native_calibration_area.shp")
sf_Dist <- st_read(dsn = path.expand(paste("data/", sp, "/", spfoldername, "/data_0.shp", sep = "")))
# sf_starNat <- st_union(sf_Dist[c(1,2),])
# sf_starInv <- st_union(sf_Dist[c(4,5),])
sf_map <- st_read("data/ne_10m_coastline/ne_10m_coastline.shp")

# nativeNZ ----------------------------------------------------------
## Read in projection
mod_proj_natNZ <- raster::stack(paste(sp, "/proj_", nativeNZrunName, "_proj_", mod, "/proj_", nativeNZrunName, "_proj_", mod, "_", sp, ".grd", sep = ""))
plot(mod_proj_natNZ[[1]])
## Determine threshold for nativeNZ model ---------------------------
# Read in model inputs to get coordinates for native data
load(paste("data/myna/BIOMOD/nativeNZ/nativeNZ_myBiomodData.Rdata", sep = ""))
NatNZcoord <- myBiomodData@coord[(!is.na(myBiomodData@data.species)) & (myBiomodData@data.species == 1),] 
rm(myBiomodData)
gc()
## MTP, P1, P5, and P10
natNZMTP <- sdm_threshold(sdm = mod_proj_natNZ[[2]], occs = NatNZcoord, percentile = 0, binary = T)
natNZP1 <- sdm_threshold(sdm = mod_proj_natNZ[[2]], occs = NatNZcoord, percentile = 1, binary = T)
natNZP5 <- sdm_threshold(sdm = mod_proj_natNZ[[2]], occs = NatNZcoord, percentile = 5, binary = T)
natNZP10 <- sdm_threshold(sdm = mod_proj_natNZ[[2]], occs = NatNZcoord, percentile = 10, binary = T)

plot(natNZMTP$rast)
plot(natNZP1$rast)
plot(natNZP5$rast)
plot(natNZP10$rast)

load(paste("data/", sp, "/BIOMOD/nativeNZ/", nativeNZrunName, "/myBiomodModelOut_", nativeNZrunName, ".Rdata", sep = ""))
get_evaluations(myBiomodModelOut)
natNZ_eval_natNZ <- get_evaluations(myBiomodModelOut) 
natNZevalnatNZcut <- natNZ_eval_natNZ["ROC","Cutoff",mod,,2] # Get the threshold/Cutoff from ROC MAXENT.Phillips, PA = 2

rast_proj_natNZ <- mod_proj_natNZ[[2]]
values(rast_proj_natNZ) <- as.numeric(values(rast_proj_natNZ) >= natNZevalnatNZcut)
# plot(rast_proj_natNZ)

# Export nativeNZ model NZ maps -------------------------------------
# Plots to make:
# 1A. NZ continuous map
# 1B. NZ binary, max sens. + spec. threshold
# 2A. NZ binary, MTP
# 2B. NZ binary, P1
# 3A. NZ binary, P5
# 3B. NZ binary, P10
pdf_nativeNZv1_NZ <- "results/myna/BIOMOD/nativeNZ/nativeNZv1a_NZ_map_PA2.pdf"
pdf(pdf_nativeNZv1_NZ, width = 11.7, height = 8.3)
par(mfrow = c(1,2), mar = c(0.25, 4, 2.5, 4), oma = c(0, 0, 2.5, 0))
# 1A
plot(terra::crop(rast(mod_proj_natNZ[[2]]), ext(sf_NZ)), axes = F, main = "Continuous")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 1B
plot(terra::crop(rast(rast_proj_natNZ), ext(sf_NZ)), axes = F, main = "Binary, max sens. + spec. threshold")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, NZ", outer = T)
# 2A
plot(terra::crop(rast(natNZMTP$rast), ext(sf_NZ)), axes = F, main = "Binary, MTP")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 2B
plot(terra::crop(rast(natNZP1$rast), ext(sf_NZ)), axes = F, main = "Binary, P1")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, NZ", outer = T)
# 3A
plot(terra::crop(rast(natNZP5$rast), ext(sf_NZ)), axes = F, main = "Binary, P5")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 3B
plot(terra::crop(rast(natNZP10$rast), ext(sf_NZ)), axes = F, main = "Binary, P10")
plot(st_geometry(sf_NZ), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, NZ", outer = T)
dev.off()


# Export nativeNZ model native maps ---------------------------------
# Plots to make:
# 1A. native continuous map
# 1B. native binary, max sens. + spec. threshold
# 2A. native binary, MTP
# 2B. native binary, P1
# 3A. native binary, P5
# 3B. native binary, P10
pdf_nativeNZv1_nat <- "results/myna/BIOMOD/nativeNZ/nativeNZv1a_native_map_PA2.pdf"
pdf(pdf_nativeNZv1_nat, height = 11.7, width = 8.3)
par(mfrow = c(2,1), mar = c(0.25, 4, 2.5, 4), oma = c(0, 0, 2.5, 0))
# 1A
plot(terra::crop(rast(mod_proj_natNZ[[2]]), ext(sf_nat)), axes = F, main = "Continuous")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 1B
plot(terra::crop(rast(rast_proj_natNZ), ext(sf_nat)), axes = F, main = "Binary, max sens. + spec. threshold")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, native range", outer = T)
# 2A
plot(terra::crop(rast(natNZMTP$rast), ext(sf_nat)), axes = F, main = "Binary, MTP")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 2B
plot(terra::crop(rast(natNZP1$rast), ext(sf_nat)), axes = F, main = "Binary, P1")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, native range", outer = T)
# 3A
plot(terra::crop(rast(natNZP5$rast), ext(sf_nat)), axes = F, main = "Binary, P5")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
# 3B
plot(terra::crop(rast(natNZP10$rast), ext(sf_nat)), axes = F, main = "Binary, P10")
plot(st_geometry(sf_map), add = T, lwd = 0.05)
plot(st_geometry(sf_Dist), add = T, lwd = 0.05)
patternLayer(sf_Dist, "diamond", density = 2, add = T)
title("v1a nativeNZ full model PA2, native range", outer = T)
dev.off()