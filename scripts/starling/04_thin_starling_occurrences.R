#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 1) {
  stop("One argument must be supplied as follows: \n
       range (e.g. native or NZ) \n", call.=FALSE)
} else {
  range <- args[1]  # "native"
}
# 04_thin_starling_occurrences.R
# Load prerequisite libraries ---------------------------------------
library(sf)
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset. 
# Define function ---------------------------------------------------
# Define a function for thin dataset when spThin uses too much RAM
humboldt.occ.rarefy2 <- function(in.pts, colxy = 2:3, rarefy.dist = 0, rarefy.units = "km", run.silent.rar = F) {
  # This function is taken from Humboldt
  # However, the distance calculated is using the dismo::Mercator function
  # rather than transforming using spTransform to a AEQD projection 
  # from c(0,0)
  # 
  # This seems to be more accurate and similar to spThin. Although test with NZ 
  # starling dataset yields about 10% more points.
  #
  require(sp)
  require(spatstat)
  switch(Sys.info()[['sysname']],
         Windows= {userOS=1},
         Linux  = {userOS=2},
         Darwin = {userOS=2})
  
  if (rarefy.units == "KM"){rarefy.units = "km"}
  if (rarefy.units == "Km"){rarefy.units = "km"}
  if (rarefy.units == "DD"){rarefy.units = "dd"}
  if (rarefy.units == "Dd"){rarefy.units = "dd"}
  
  if (rarefy.units == "km") {
    min.dist <- rarefy.dist * 1000  #values in km
    sp2p1 <- SpatialPoints(in.pts[, colxy], CRS("+proj=longlat +datum=WGS84"))
    # sp2p2 <- spTransform(sp2p1, CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    # xy <- data.frame(x4r = coordinates(sp2p2)[, 1], y4r = coordinates(sp2p2)[, 2])
    coords <- dismo::Mercator(coordinates(sp2p1)) 
    xy <- data.frame(x4r = coords[, 1], y4r = coords[, 2])
    xy <- data.frame(cbind(xy,in.pts))#new
  }
  
  if (rarefy.units == "dd") {
    yy <- colxy[2]
    maxLat <- max(in.pts[, yy])
    minLat <- min(in.pts[, yy])
    # estimage dd to km for study area
    rare.dd <- (mean(c(maxLat, minLat)))
    adjKm <- (-0.0139 * (rare.dd * rare.dd)) + (0.0898 * rare.dd) + 111.1
    min.dist <- adjKm * rarefy.dist * 1000  #values in km
    print(paste("Value used for rarefying:", round((min.dist/1000), 2), "km. Remember that the length of a decimal degrees changes latitudinally due to the convergence of the lines of longitude at the poles. The value used here is the average distance of decimal-degrees within your study area. Alternatively, simply input distance as km value and change rarefy.units='km'"))
    sp2p1 <- SpatialPoints(in.pts[, colxy], CRS("+proj=longlat +datum=WGS84"))
    sp2p2 <- spTransform(sp2p1, CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    xy <- data.frame(x4r = coordinates(sp2p2)[, 1], y4r = coordinates(sp2p2)[, 2])
  }
  
  nPts<-nrow(xy)
  # if (run.silent.rar == F & userOS==1){pb <- winProgressBar(title = "Initializing",min = 0,max =nPts, width = 300)}
  if (run.silent.rar == F & userOS==2){print("No progress bar because no tcltk package")}
  # setup env- sepaerate from distance
  spName <- in.pts[, 1][1]
  new.data <- NULL
  
  to <- 1
  del.min.dist <- xy
  
  repeat {
    nn1 <- nndist(del.min.dist[, "x4r"], del.min.dist[, "y4r"])  # calculate distance nearest neighbour
    if (sum(nn1 < min.dist) == 0) {
      break
    }
    
    # iteratively removing points starting with the one having the minimal distance to the
    # nearest neighbour
    nn2 <- nndist(del.min.dist[, "x4r"], del.min.dist[, "y4r"], k = 2)
    
    del1 <- nn1 == min(nn1)
    del2 <- nn2 == min(nn2[del1])
    delk <- del1 & del2
    if (sum(del2) > 1) {
      for (k in 3:6) {
        nn <- nndist(del.min.dist[, "x4r"], del.min.dist[, "y4r"], k = k)
        delk <- delk & nn == min(nn[delk])
        # if (run.silent.rar == F & userOS==1){setWinProgressBar(pb, length(delk), title=paste(" Rarefying:",length(delk),"remaining localities (of", nPts,"input)"))}
        # if (run.silent.rar == F & userOS==2){setTkProgressBar(pb, length(delk), title=paste(length(delk),"remaining pts (of", nPts))}
        if (sum(nn[delk] == min(nn[delk])) > 1) {
          break
        }
      }
    }
    # from the two points which are the nearest neighbours of the whole set, remove the one
    # closest to the second neighbour
    del.min.dist <- del.min.dist[-(which(delk)[1]), ]
  }
  # if (run.silent.rar == F){close(pb)}
  new.data <- rbind(new.data, del.min.dist)
  nc<-(ncol(new.data))
  col.orig<-c(3:nc)
  data.out<-new.data[,col.orig]
  print(paste("Starting points =", nrow(xy), ", Final rarefied points =", nrow(new.data)))
  return(data.out)
}
# Reads in occurrence points recurring localities -------------------
# Note that the work flow is a little different from the myna 
# dataset. In the myna work flow, the definition of recurring localities
# was done in the 04*_thin_myna_*.R scripts, while the same process
# was done in the 03*i_further_clean_*_starling_occurrences.R script
# This was to compartmentalise the process.
#
inputcsv <- paste("data/starling/filtered_occurrences/Starling_", range, "_breeding_1971-2020_32km4yr_recurring_cellwithStarling.csv", sep = "")
df_cellWithstarling <- fread(inputcsv)

# Thin dataset ------------------------------------------------------
# Thin per 32km cluster
dir.create(paste("data/starling/filtered_occurrences/", range, "_thinned/32km", sep = ""), recursive = T, showWarnings = F)

for (clust in unique(df_cellWithstarling$Clust32km)){
  nkm <- 32
  outFile <- paste("starling_", range, "_32kmthinned_clust", sprintf("%04d", clust), sep = "")
  print(outFile)
  df_subset <- df_cellWithstarling[df_cellWithstarling$Clust32km == clust,]
  nrep <- nrow(df_subset)
  print(nrep)
  if (nrep == 1){
    write_csv(df_subset[,c("species", "x", "y")], 
              file = paste("data/starling/filtered_occurrences/", range, "_thinned/32km/", outFile, "_thin1.csv", sep = ""),
              quote = "none")
  }  else { #if (nrep < 30000) {
    spThin::thin(df_cellWithstarling[df_cellWithstarling$Clust32km == clust,],
                 lat.col = "y",
                 long.col = "x",
                 spec.col = "species",
                 thin.par = nkm,  # 16km as this is the supposed home range
                 reps = 25,
                 locs.thinned.list.return = F,  # False so that the records are written into file directly
                 write.files = TRUE,
                 max.files = 1,
                 out.dir = paste("data/starling/filtered_occurrences/", range, "_thinned/32km/", sep = ""),
                 out.base = outFile,
                 write.log.file = F)
  # } else {
  #   df <- humboldt.occ.rarefy2(as.data.frame(df_cellWithstarling[df_cellWithstarling$Clust32km == clust,]), 
  #                              colxy = 1:2, rarefy.dist = nkm, rarefy.units = "km")
  #   write_csv(df[,c("species", "x", "y")], 
  #             file = paste("data/starling/filtered_occurrences/", range, "_thinned/32km/", outFile, "_thin1.csv", sep = ""),
  #             quote = "none")
  }
}


