#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 3) {
  stop("Three argument must be supplied as follows: \n
       sp (e.g. myna) \n
       runName (e.g. nativeNZ_BIOMODv1a)\n
       P_val (e.g. 5)\n", call.=FALSE)
} else {
  sp <- args[1]  # "myna"
  runName <- args[2]  # nativeNZ_BIOMODv1a
  P_val <- as.numeric(args[3])  # training presence threshold 5
}
# compare_future_present_proj.R -------------------------------------
# Libraries ---------------------------------------------------------
library(raster)
library(biomod2)
library(terra)
library(tidyverse)
library(sf)
# Define input ------------------------------------------------------
# P_val <- 10  # % training presence threshold
# Functions ---------------------------------------------------------
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
    values(sdm_thresh)  <- as.numeric(terra::values(sdm_thresh) >= thresh)
  } else {
    sdm_thresh[sdm_thresh < thresh] <- NA
  }
  print(thresh)
  return(list(threshold = thresh, rast = sdm_thresh))
}


rast_binary_thres <- function(rast, threshold){
  r <- rast
  values(r) <- as.numeric(terra::values(r) >= threshold)
  return(r)
}

# Read in rasters ---------------------------------------------------
r_pres <- raster::stack(paste(sp, "/proj_", runName,"_proj_MAXENT.Phillips/proj_", runName,"_proj_MAXENT.Phillips_", sp, ".grd", sep = ""))
r_ssp126_ssp1 <- raster::stack(paste(sp, "/proj_", runName,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips_", sp, ".grd", sep = ""))
r_ssp370_ssp3 <- raster::stack(paste(sp, "/proj_", runName,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips_", sp, ".grd", sep = ""))
r_ssp370_none <- raster::stack(paste(sp, "/proj_", runName,"_projfutureNZ_ssp370_none_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp370_none_MAXENT.Phillips_", sp, ".grd", sep = ""))
outline <- st_read("data/myna/NZ_calibration_area/NZ_calibration_area.shp")
# Determine threshold -----------------------------------------------
load(paste("data/", sp, "/BIOMOD/NZ/NZ_myBiomodData.Rdata", sep = ""))
NZcoord <- myBiomodData@coord[(!is.na(myBiomodData@data.species)) & (myBiomodData@data.species == 1),] 
# Clear up some memory if possible
rm(myBiomodData)
gc()

NZP <- sdm_threshold(sdm = r_pres[[2]], occs = NZcoord, percentile = P_val, binary = T)
r1 <- crop(NZP$rast, extent(r_ssp126_ssp1))

#plot(r1)

r2 <- rast_binary_thres(r_ssp126_ssp1[[2]], NZP$threshold)
r3 <- rast_binary_thres(r_ssp370_ssp3[[2]], NZP$threshold)
r4 <- rast_binary_thres(r_ssp370_none[[2]], NZP$threshold)

r2 <- terra::rast(raster::reclassify(r2, cbind(1, 2)))
r3 <- terra::rast(raster::reclassify(r3, cbind(1, 4)))
r4 <- terra::rast(raster::reclassify(r4, cbind(1, 8)))

r1 <- terra::rast(r1)
rr <- terra::rast(list(r1, r2, r3, r4))
datasum<- app(rr, fun = sum)
values(datasum) <- as.factor(terra::values(datasum))

rr <- terra::rast(list(r1, r2, r3))
datasum2 <- app(rr, fun = sum)
values(datasum2) <- as.factor(terra::values(datasum2))
# plot(datasum)
# 15 = all
# 14 = all except present = area gained in all future scenarios
# 13 = all except ssp126/1
# 12 = all except present and ssp126/1 = area gained in all ssp370 scenarios
# 8 = all except present, ssp126/1, and ssp370/3 = area gained in ssp370/none
# 4 = all except present, ssp126/1, and ssp370/none = area gained in ssp370/3
range <- strsplit(runName, split = "_")[[1]][1]
dir.create(paste(path = "results/", sp, "/BIOMOD/", range, "/future_projections/", sep = ""), showWarnings = F, recursive = T, )
outpdf <- paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ.pdf", sep = "")
pdf(outpdf, width = 11.7, height = 8.3)
par(mfrow = c(1,2), mar = c(0.25, 4, 2.5, 4), oma = c(0, 0, 2.5, 0))
plot(r1, axes = F, main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""), legend = F)
plot(st_geometry(outline), add = T)
plot(r2, axes = F, main = "ssp126 climate, ssp1 pop_dens", legend = F)
plot(st_geometry(outline), add = T)
plot(r3, axes = F, main = "ssp370 climate, ssp3 pop_dens", legend = F)
plot(st_geometry(outline), add = T)
plot(r4, axes = F, main = "ssp370 climate, present day pop_dens", legend = F)
plot(st_geometry(outline), add = T)
par(mfrow=c(1,1))
plot(datasum, main = "Area gains/losses", legend = T, col = c("white", "yellow", "green", "forestgreen", "blue", "red"))
legend("topleft", title = "Projected suitable areas",
       legend = c("None",
                  "Present and future scenarios",
                  "All future scenarios",
                  "Only SSP126 scenarios",
                  "Only SSP370 climate + present day population density",
                  "Only SSP370 climate + SSP3 population density"),
       fill = c("white", "red", "blue", "forestgreen", "green", "yellow"))
plot(st_geometry(outline), add = T)
# plot(datasum2, main = "Area gains/losses", legend = F, col = c(NA, "#eab64f", "#8dd100", "#00a600"), axes = F)
# legend("topleft", #title = "Areas",
#        legend = c("Present day",
#                   "SSP126",
#                   "SSP370"),
#        fill = c("#00a600", "#8dd100", "#eab64f"))
# plot(st_geometry(outline), add = T)
dev.off()

colpal <- colorRampPalette(colors = c("#8dd100", "#eab64f", "#ffeda0"))
# The values are quite different so it is easer to do it specifically for each species. 
# Note that different threshold may require tinkering with the labels of the plots.
png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_onepanel.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
plot(r1, 
     xlim = c(165, 210),
     ylim = c(-50, -32.5),
     axes = F, 
     # main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""), 
     legend = F)
plot(st_geometry(outline), add = T)
text(x = 170, y = -33, labels = "A) Present", cex = 1.5)

plot(shift(r2, dx = 10), 
     axes = F, 
     # main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""), 
     legend = F,
     add = T)
plot(st_geometry(outline) + c(10, 0 ), add = T)
text(x = 177, y = -48, labels = "SSP126", srt = 53)

plot(shift(r3, dx = 15), 
     axes = F, 
     # main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""),
     legend = F, 
     add = T)
plot(st_geometry(outline) + c(15, 0 ), add = T)
text(x = 182, y = -48, labels = "SSP370A", srt = 53)

plot(shift(r4, dx = 20), 
     axes = F, 
     # main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""),
     legend = F, 
     add = T)
plot(st_geometry(outline) + c(20, 0 ), add = T)

text(x = 185, y = -33, labels = "B) Future", cex = 1.5)
text(x = 187, y = -48, labels = "SSP370B", srt = 53)
if (sp == "myna"){
  plot(shift(datasum, dx = 30),
       axes = F,
       # main = "Area gains/losses", 
       legend = F, 
       # breaks = c(0, 4, 8, 13, 14, 15),
       col = c("white", rev(colpal(4)), "#00a600"),
       # col = c("white", "#ccece6", "#99d8c9", "#66c2a4", "#2ca25f", "#006d2c"),
       # col = c("white", "yellow", "green", "forestgreen", "blue", "red"), 
       add = T)
  plot(st_geometry(outline) + c(30, 0 ), add = T)
  
  legend("bottomright", title = "Present + future",
         legend = rev(c("Present",
                        "All future scenarios",
                        "All SSP370 scenarios",
                        "SSP370B",
                        "SSP370A",
                        "Not suitable")),
         fill = c("white", rev(colpal(4)), "#00a600"))
  # fill = c("white", "#ccece6", "#99d8c9", "#66c2a4", "#2ca25f", "#006d2c")) #,
  # fill = c("white", "red", "blue", "forestgreen", "green", "yellow"))
  
  text(x = 202, y = -33, labels = "C) Present + future", cex = 1.5)
  # box()
  
} else if (sp == "starling"){
  plot(shift(datasum, dx = 30),
       axes = F,
       # main = "Area gains/losses", 
       legend = F, 
       # breaks = c(0, 4, 8, 13, 14, 15),
       col = c("white", rev(colpal(4)), "#00a600"),
       add = T)
  plot(st_geometry(outline) + c(30, 0 ), add = T)
  
  legend("bottomright", title = "Present + future",
         legend = rev(c("Present",
                        "All future scenarios",
                        "All SSP370 scenarios",
                        "SSP370B",
                        "SSP126",
                        "Not suitable")),
         fill = c("white", rev(colpal(4)), "#00a600"))
  
  text(x = 202, y = -33, labels = "C) Present + future", cex = 1.5)
}

dev.off()


dt_order <- data.frame(val = c(0, 2, 4, 8, 12, 13, 14, 15),
                       lab = c("Not suitable",
                               "SSP126",
                               "SSP370A",
                               "SSP370B",
                               "All SSP370 scenarios",
                               "Area lost by SSP126",
                               "All future scenarios", 
                               "Present"))

rev(c("Present day",
"Area gained by all future scenarios",
"Area gained by all SSP370 scenarios",
"Area gained by only SSP370 climate and present day pop. density",
"Area gained by only SSP370 climate and SSP3 pop. density",
"Area not suitable in all scenarios"))

val_ls <- c(0, 4, 8, 12, 13, 14, 15)
col_ls <- c("#f7fcfd", "#e5f5f9", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#006d2c", "#00441b")
            
png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_P1.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
par(mfrow = c(1,2), mar = c(0.25, 4, 2.5, 4), oma = c(0, 0, 2.5, 0))
plot(r1, axes = F, main = paste("Present-day model, binary, P", P_val, " threshold", sep = ""), legend = F)
plot(st_geometry(outline), add = T)
plot(r2, axes = F, main = "ssp126 climate, ssp1 pop_dens", legend = F)
plot(st_geometry(outline), add = T)
dev.off()
png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_P2.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
par(mfrow = c(1,2), mar = c(0.25, 4, 2.5, 4), oma = c(0, 0, 2.5, 0))
plot(r3, axes = F, main = "ssp370 climate, ssp3 pop_dens", legend = F)
plot(st_geometry(outline), add = T)
plot(r4, axes = F, main = "ssp370 climate, present day pop_dens", legend = F)
plot(st_geometry(outline), add = T)
dev.off()
png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_P3.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
plot(datasum, main = "Area gains/losses", legend = T, col = c("white", "yellow", "green", "forestgreen", "blue", "red"))
legend("topleft", title = "Projected suitable areas",
       legend = c("None",
                  "Present and future scenarios",
                  "All future scenarios",
                  "Only SSP370 scenarios",
                  "Only SSP370 climate + present day population density",
                  "Only SSP370 climate + SSP3 population density"),
       fill = c("white", "red", "blue", "forestgreen", "green", "yellow"))
plot(st_geometry(outline), add = T)
dev.off()

png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_P4.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
plot(datasum2, main = "Area gains/losses", legend = F, col = c(NA, "#eab64f", "#8dd100", "#00a600"), axes = F)
legend("topleft", #title = "Areas",
       legend = c("Present day",
                  "SSP126",
                  "SSP370"),
       fill = c("#00a600", "#8dd100", "#eab64f"))
plot(st_geometry(outline), add = T)
dev.off()

# For NZ model starlings --------------------------------------------
# png(paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ_P4.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
# plot(datasum2, main = "Area gains/losses", legend = F, col = c(NA, "#eab64f", "red", "#8dd100", "#00a600"), axes = F)
# legend("topleft", #title = "Areas",
#        legend = c("Present day",
#                   "Loss under SSP126",
#                   "SSP126",
#                   "SSP370"),
#        fill = c("#00a600", "red", "#8dd100", "#eab64f"))
# plot(st_geometry(outline), add = T)
# dev.off()

# terra::globa(r==1, sum, na.rm=TRUE)
# Convert zeros to NA
# Calculate area, count number of cells
sum(terra::values(terra::cellSize(datasum, mask = T, unit = "km", rcx = max(c(terra::nrow(datasum), terra::ncol(datasum))))), na.rm = T)
# Number of cells gained at each stage.
dt_sum <- data.frame(area = c("Present scenarios",
                    "Area gained by all future scenarios",
                    "Area lost by SSP126 scenario",
                    "Area gained by all SSP370 scenarios",
                    "Area gained by only SSP370 climate and present day pop. density",
                    "Area gained by only SSP370 climate and SSP3 pop. density",
                    "Area gained by only SSP126 scenario",
                    "Area not suitable in all scenarios"),
           cells = c(
terra::global(datasum == "15", sum, na.rm = T)[[1]], 
terra::global(datasum == "14", sum, na.rm = T)[[1]], 
terra::global(datasum == "13", sum, na.rm = T)[[1]], 
terra::global(datasum == "12", sum, na.rm = T)[[1]], 
terra::global(datasum == "8", sum, na.rm = T)[[1]], 
terra::global(datasum == "4", sum, na.rm = T)[[1]], 
terra::global(datasum == "2", sum, na.rm = T)[[1]], 
terra::global(datasum == "0", sum, na.rm = T)[[1]]),
           area_km = c(
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "15") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "14") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "13") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "12") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "8") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "4") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "2") == 1], na.rm = T),
sum(terra::values(terra::cellSize(datasum, unit = "km"))[values(datasum == "0") == 1], na.rm = T))
)
# sum(terra::values(terra::cellSize(datasum, unit = "km")), na.rm = T)
# terra::globa(datasum %in% c("15", "14", "12", "8", "4", "0"), sum, na.rm = T)
# 86548.03 + 24818.9 + 40702.79 + 377.6573 + 9.599843 + 99766.18
write_csv(dt_sum, file = paste("results/", sp, "/BIOMOD/", range, "/future_projections/", sp, "_", runName, "_future_P", P_val, "_NZ.csv", sep = ""))
