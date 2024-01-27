# plot_clamping_masks.R ---------------------------------------------
#
# Libraries ---------------------------------------------------------
library(raster)
library(biomod2)
library(terra)
library(tidyverse)
library(sf)

# Define inputs -----------------------------------------------------
sp <- "myna"
runName <- "nativeNZ_fullBIOMODv1a"
sp2 <- "starling"
runName2 <- "NZ_fullBIOMODv1"
# Read in rasters ---------------------------------------------------
# r_pres <- raster::stack(paste(sp, "/proj_", runName,"_proj_MAXENT.Phillips/proj_", runName,"_proj_MAXENT.Phillips_", sp, ".grd", sep = ""))
# Myna
r_ssp126_ssp1 <- terra::rast(paste(sp, "/proj_", runName,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips_ClampingMask.grd", sep = ""))
r_ssp370_ssp3 <- terra::rast(paste(sp, "/proj_", runName,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips_ClampingMask.grd", sep = ""))
r_ssp370_none <- terra::rast(paste(sp, "/proj_", runName,"_projfutureNZ_ssp370_none_MAXENT.Phillips/proj_", runName,"_projfutureNZ_ssp370_none_MAXENT.Phillips_ClampingMask.grd", sep = ""))
# Starling
r_ssp126_ssp1_star <- terra::rast(paste(sp2, "/proj_", runName2,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips/proj_", runName2,"_projfutureNZ_ssp126_ssp1_MAXENT.Phillips_ClampingMask.grd", sep = ""))
r_ssp370_ssp3_star <- terra::rast(paste(sp2, "/proj_", runName2,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips/proj_", runName2,"_projfutureNZ_ssp370_ssp3_MAXENT.Phillips_ClampingMask.grd", sep = ""))
r_ssp370_none_star <- terra::rast(paste(sp2, "/proj_", runName2,"_projfutureNZ_ssp370_none_MAXENT.Phillips/proj_", runName2,"_projfutureNZ_ssp370_none_MAXENT.Phillips_ClampingMask.grd", sep = ""))

outline <- st_read("data/myna/NZ_calibration_area/NZ_calibration_area.shp")

# Make plots
# Note that all raster has values from 0-4, which means that we don't need
# to fidget much with the colour legend, but I specified the breaks and the
# colour here anyways to make sure that the codes are transferable.
png(paste("results/supplementary/", sp, sp2, "_future_ClampingMask_NZ_onepanel.png", sep = ""), width = 11.7, height = 8.3, res = 300, units = "in", bg = "transparent")
plot(r_ssp126_ssp1, 
     xlim = c(165, 210),
     ylim = c(-50, -32.5),
     axes = F, 
     legend = F,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline), add = T)
text(x = 167, y = -48, labels = "SSP126", srt = 53)
text(x = 174.5, y = -33, labels = "A) Myna", cex = 1.5)

plot(shift(r_ssp370_ssp3, dx = 5), 
     axes = F, 
     legend = F,
     add = T,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline) + c(5, 0 ), add = T)
text(x = 172, y = -48, labels = "SSP370A", srt = 53)

plot(shift(r_ssp370_none, dx = 10), 
     axes = F, 
     legend = F, 
     add = T,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline) + c(10, 0 ), add = T)
text(x = 177, y = -48, labels = "SSP370B", srt = 53)



# Starling
plot(shift(r_ssp126_ssp1_star, dx = 20), 
     axes = F, 
     legend = F, 
     add = T,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline) + c(20, 0 ), add = T)

text(x = 195, y = -33, labels = "B) Starling", cex = 1.5)
text(x = 187, y = -48, labels = "SSP126", srt = 53)

plot(shift(r_ssp370_ssp3_star, dx = 25),
     axes = F,
     legend = F, 
     add = T,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline) + c(25, 0 ), add = T)
text(x = 192, y = -48, labels = "SSP370A", srt = 53)

plot(shift(r_ssp370_none_star, dx = 30),
     axes = F,
     legend = F, 
     add = T,
     breaks = seq(0, 5, length.out = 6) - 0.5,
     col = rev(grDevices::terrain.colors(5)))
plot(st_geometry(outline) + c(30, 0 ), add = T)
text(x = 197, y = -48, labels = "SSP370B", srt = 53)

legend(x = 203, y = -45, title = "No. of variables",
       legend = c("0",
                      "1",
                      "2",
                      "3",
                      "4"),
       fill = rev(grDevices::terrain.colors(5)))


dev.off()