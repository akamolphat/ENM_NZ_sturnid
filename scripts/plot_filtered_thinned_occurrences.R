# plot_filtered_thinned_occurrences.R -------------------------------
#
# Make Figure 1 of the main text
#
# Note that the occurrence points in the *_thinned_merged.csv files
# are not exactly what was used in the BIOMOD models as some of the 
# points fell on the edges are on gridcells with NA values.
# Therefore, the points are taken from BIOMOD object
#
# Load libraries ----------------------------------------------------
library(sf)  # manipulating shapefiles
library(terra)
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)

# Read in shapefiles ------------------------------------------------
sf_coastline <- st_read(dsn = path.expand("data/ne_10m_coastline/ne_10m_coastline.shp"))  # Coastline for plotting
sf_mynacal <- st_read("data/myna/native_calibration_area/native_calibration_area.shp")    # Myna native calibration area
sf_starcal <- st_read("data/starling/native_calibration_area/starling_native_calibration_area.shp")   # Starling native calibration area
sf_NZ <-st_read("data/myna/NZ_calibration_area/NZ_calibration_area.shp")                  # NZ calibration area
# Myna --------------------------------------------------------------
sf_mynaDist <- st_read(dsn = path.expand("data/myna/A_tristis_distribution_map/data_0.shp"))
sf_mynaNat <- sf_mynaDist[1,]  # First feature is the native range polygon
sf_mynaInv <- sf_mynaDist[2,]  # First feature is the Invasive range polygon
## Native range -----------------------------------------------------
### Read in occurrence points from BIOMOD objects -------------------
load("data/myna/BIOMOD/native/native_myBiomodDatav1.Rdata")  # loads myBiomodData object
sf_myna_nat <- myBiomodData@coord[myBiomodData@data.species == 1 & (!is.na(myBiomodData@data.species)),] %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_coastline))
rm(myBiomodData)
## NZ ---------------------------------------------------------------
### Read in occurrence points from BIOMOD objects -------------------
load("data/myna/BIOMOD/NZ/NZ_myBiomodDatav1.Rdata")  # loads myBiomodData object
sf_myna_NZ <- myBiomodData@coord[myBiomodData@data.species == 1 & (!is.na(myBiomodData@data.species)),] %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_coastline))
rm(myBiomodData)




# dir.create("results/species_dist", showWarnings = F, recursive = T)
# png("results/species_dist/myna_NZ_dist.png", width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
# plot(st_geometry(sf_NZ))
# plot(st_geometry(sf_mynaDist), col = "#d95f02", add = T)
# dev.off()
# Starling ----------------------------------------------------------
sf_starDist <- st_read(dsn = path.expand("data/starling/S_vulgaris_distribution_map/data_0.shp"))
sf_starbreed <- sf_starDist[c(1,2,4,5),]
## Native range -----------------------------------------------------
### Read in occurrence points from BIOMOD objects -------------------
load("data/starling/BIOMOD/native/native_myBiomodData.Rdata")  # loads myBiomodData object
sf_star_nat <- myBiomodData@coord[myBiomodData@data.species == 1 & (!is.na(myBiomodData@data.species)),] %>%
  st_as_sf(coords = c("X", "Y"), crs = crs(sf_coastline))  # NOTE that "X" and "Y" are capitalised compared to other myBiomodData object
rm(myBiomodData)
## NZ ---------------------------------------------------------------
### Read in occurrence points from BIOMOD objects -------------------
load("data/starling/BIOMOD/NZ/NZ_myBiomodData.Rdata")  # loads myBiomodData object
sf_star_NZ <- myBiomodData@coord[myBiomodData@data.species == 1 & (!is.na(myBiomodData@data.species)),] %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_coastline)) 
rm(myBiomodData)
# Read in png of mynas and starlings to add to map -----------------
# library(png)
# pngMyna <- readPNG("data/Myna.png")
# pngStar <- readPNG("data/starling_blackandwhite_cropped.png")
# Plot maps all in one png file ------------------------------------
dir.create(path = "results/main_text/", showWarnings = F, recursive = T)
NZxbrk <- seq(168, 178, by = 5)
NZxlab <- paste(NZxbrk, "\u00B0", "E", sep = "")
NZybrk <- seq(-46, -34, by = 5)
NZylab <- paste(seq(46, 34, by = -5), "\u00B0", "S", sep = "")

ptpchls <- c(4, 2, 15, 16, 17, 18)
for (ptpch in ptpchls){
  # ctr <- 0
  ptsizecex <- 0.6
  png(paste("results/main_text/Figure1_pch", ptpch, ".png", sep = ""), height = 9, width = 8.3, res = 300, units = "in", 
      # bg = "transparent"
  )
  par(mai = c(0.5, 0.4, 0.1, 0.1))
  
  nf <- layout(
    mat = matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), 
    widths=c(3,2), 
    heights=c(6,6,1)
  )
  # myna native
  plot(st_geometry(sf_mynacal), border = "#d95f02")
  plot(st_geometry(sf_mynaNat), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_nat), pch = ptpch, col = alpha("black", 0.4), cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_mynacal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(60, 120, by = 30), labels = paste(seq(60, 120, by = 30), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 60, by = 15), labels = paste(seq(0, 60, by = 15), "\u00B0", "N", sep = ""))
  box()
  # mtext(col="black", expression(bold("(A)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("A)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # myna NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_mynaInv), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_NZ), pch = ptpch, 
       # col = alpha("black", 0.4), 
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  # mtext(col="black", expression(bold("(B)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("B)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # star native
  plot(st_geometry(sf_starcal), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_nat), pch = ptpch, 
       col = alpha("black", 0.4),
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_starcal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(0, 120, by = 60), labels = paste(seq(0, 120, by = 60), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 80, by = 40), labels = paste(seq(0, 80, by = 40), "\u00B0", "N", sep = ""))
  box()
  mtext(col="black", expression(bold("C)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  
  # star NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_NZ), pch = ptpch, 
       # col = alpha("black", 0.4), 
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  mtext(col="black", expression(bold("D)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # dev.off()
  # legend only panel
  par(mai = c(0.1, 0.4, 0.1, 0.1))
  plot.new()
  # box()
  legend("top", horiz = T,
         legend = c("Distribution", "Calibration area", "Occurrences"),       
         cex = 1.8, 
         fill = c("#00a600", NA, NA),
         border = c(NA, "#d95f02", NA),
         pch = c(NA,NA,ptpch)) # Color of the border of the shapes
  
  dev.off()
}

ptsizecexls <- c(0.6, 0.8, 1)
for (ptsizecex in ptsizecexls){
  # ctr <- 0
  ptpch <- 18
  png(paste("results/main_text/Figure1_cex", ptsizecex, ".png", sep = ""), height = 9, width = 8.3, res = 300, units = "in", 
      # bg = "transparent"
  )
  par(mai = c(0.5, 0.4, 0.1, 0.1))
  
  nf <- layout(
    mat = matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), 
    widths=c(3,2), 
    heights=c(6,6,1)
  )
  # myna native
  plot(st_geometry(sf_mynacal), border = "#d95f02")
  plot(st_geometry(sf_mynaNat), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_nat), pch = ptpch, col = alpha("black", 0.4), cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_mynacal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(60, 120, by = 30), labels = paste(seq(60, 120, by = 30), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 60, by = 15), labels = paste(seq(0, 60, by = 15), "\u00B0", "N", sep = ""))
  box()
  # mtext(col="black", expression(bold("(A)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("A)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # myna NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_mynaInv), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_NZ), pch = ptpch, 
       # col = alpha("black", 0.4),
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  # mtext(col="black", expression(bold("(B)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("B)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # star native
  plot(st_geometry(sf_starcal), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_nat), pch = ptpch, 
       col = alpha("black", 0.4),
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_starcal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(0, 120, by = 60), labels = paste(seq(0, 120, by = 60), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 80, by = 40), labels = paste(seq(0, 80, by = 40), "\u00B0", "N", sep = ""))
  box()
  mtext(col="black", expression(bold("C)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  
  # star NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_NZ), pch = ptpch, 
       # col = alpha("black", 0.4),
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  mtext(col="black", expression(bold("D)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # dev.off()
  # legend only panel
  par(mai = c(0.1, 0.4, 0.1, 0.1))
  plot.new()
  # box()
  legend("top", horiz = T,
         legend = c("Distribution", "Calibration area", "Occurrences"),       
         cex = 1.8, 
         fill = c("#00a600", NA, NA),
         border = c(NA, "#d95f02", NA),
         pch = c(NA,NA,ptpch)) # Color of the border of the shapes
  
  dev.off()
}

# Grey points -------------------------------------------------------
ptsizecexls <- c(0.6, 0.8, 1)
for (ptsizecex in ptsizecexls){
  # ctr <- 0
  ptpch <- 18
  png(paste("results/main_text/Figure1_cex", ptsizecex, "v2.png", sep = ""), height = 9, width = 8.3, res = 300, units = "in", 
      # bg = "transparent"
  )
  par(mai = c(0.5, 0.4, 0.1, 0.1))
  
  nf <- layout(
    mat = matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), 
    widths=c(3,2), 
    heights=c(6,6,1)
  )
  # myna native
  plot(st_geometry(sf_mynacal), border = "#d95f02")
  plot(st_geometry(sf_mynaNat), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_nat), pch = ptpch, col = alpha("darkgrey", 0.5), cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_mynacal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(60, 120, by = 30), labels = paste(seq(60, 120, by = 30), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 60, by = 15), labels = paste(seq(0, 60, by = 15), "\u00B0", "N", sep = ""))
  box()
  # mtext(col="black", expression(bold("(A)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("A)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # myna NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_mynaInv), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_myna_NZ), pch = ptpch, 
       col = "darkgrey",
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  # mtext(col="black", expression(bold("(B)")), side=2, line=2, at=par('usr')[4], las=2, adj = 0.3, padj = 1, cex = 1.5)
  mtext(col="black", expression(bold("B)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # star native
  plot(st_geometry(sf_starcal), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_nat), pch = ptpch, 
       col = alpha("darkgrey", 0.4),
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_starcal), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = seq(0, 120, by = 60), labels = paste(seq(0, 120, by = 60), "\u00B0", "E", sep = ""))
  axis(side = 2, at =  seq(0, 80, by = 40), labels = paste(seq(0, 80, by = 40), "\u00B0", "N", sep = ""))
  box()
  mtext(col="black", expression(bold("C)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  
  # star NZ
  plot(st_geometry(sf_NZ), border = "#d95f02")
  plot(st_geometry(sf_starbreed), col = "#00a600", border = NA, add = T)
  plot(st_geometry(sf_star_NZ), pch = ptpch, 
       col = "darkgrey",
       cex = ptsizecex, add = T)
  plot(st_geometry(sf_coastline), add = T)
  plot(st_geometry(sf_NZ), border = "#d95f02", add = T)  # Plot again so that the colour stands out
  axis(side = 1, at = NZxbrk, labels = NZxlab)
  axis(side = 2, at = NZybrk, labels = NZylab)
  box()
  mtext(col="black", expression(bold("D)")), side=2, line=-1.2, at=par('usr')[4], las=2, adj = 0.3, padj = 1.4, cex = 1.5)
  # dev.off()
  # legend only panel
  par(mai = c(0.1, 0.4, 0.1, 0.1))
  plot.new()
  # box()
  legend("top", horiz = T,
         legend = c("Distribution", "Calibration area", "Occurrences"),       
         cex = 1.8, 
         fill = c("#00a600", NA, NA),
         border = c(NA, "#d95f02", NA),
         pch = c(NA,NA,ptpch)) # Color of the border of the shapes
  
  dev.off()
}
