# 03ai_further_clean_native_starling_occurrences.R
# Load prerequisite libraries ---------------------------------------#####
library(sf)
library(tidyverse)  # data manipulation
# library(plotly)  # some interactive plots for some basic visualisation
# library(rgeos)  # gUnaryUnion and gIntersect function for merging/clipping
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset. 
source("scripts/patternfun.R")
# Read data ---------------------------------------------------------#####
sf_dist <- st_read(dsn = path.expand("data/starling/S_vulgaris_distribution_map/data_0.shp"))
sf_starNat <- st_union(sf_dist[c(1,2),])
sf_natcal <- st_read(dsn = "data/starling/native_calibration_area/starling_native_calibration_area.shp")
sf_poly <- st_read(dsn = path.expand("data/starling/native_calibration_area/starling_native_plot_area.shp"))
sf_gbif1971_2020 <- fread("data/starling/filtered_occurrences/Starling_native_breeding_1971_2020_withclust.csv") %>%
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)
st_crs(sf_gbif1971_2020) <- "WGS84"  # or 4326
# Plot native points by defined localities ----------------------------#####
dir.create("results/starling/occurrences", recursive = T, showWarnings = F)
png("results/starling/occurrences/native_10km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust10km), add = T)
dev.off()

png("results/starling/occurrences/native_32km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust32km), add = T)
dev.off()

png("results/starling/occurrences/native_50km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust50km), add = T)
dev.off()

png("results/starling/occurrences/native_40km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust40km), add = T)
dev.off()

png("results/starling/occurrences/native_100km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust100km), add = T)
dev.off()

png("results/starling/occurrences/native_200km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust200km), add = T)
dev.off()

# 9.a.ii. Filter for occurrences from recurring locality ------------#####
# 32km cluster is used as this is the estimated homerange of the
# common myna
filter_occ_clust_yr <- function(sf, clustcol, yrthreshold){
  # sf = sf object with gbif records
  # clustcol = name of column with cluster defined (character string)
  # yrthreshold = minimum number of years of recurring records from the
  #               cluster to consider the locality recurring (numeric)
  sf$clustcol <- sf[[clustcol]]
  df_sum <- data.table(st_drop_geometry(sf))[, .(
    avg_x = mean(decimalLongitude),
    avg_y = mean(decimalLatitude),
    min_yr = min(year),
    max_yr = max(year),
    n_occ = length(year),
    yr_range = diff(range(year))
  ), keyby = .(grp = clustcol)]
  df_recurring <- df_sum[yr_range >= yrthreshold,]
  sf_recurring <- sf[sf$clustcol %in% df_recurring$grp, ]
  return(sf_recurring)
}

sf_recurring32km4yr <- filter_occ_clust_yr(sf = sf_gbif1971_2020, clustcol = "Clust32km", yrthreshold = 4)
sf_recurring32km5yr <- filter_occ_clust_yr(sf = sf_gbif1971_2020, clustcol = "Clust32km", yrthreshold = 5)
sf_recurring100km4yr <- filter_occ_clust_yr(sf = sf_gbif1971_2020, clustcol = "Clust100km", yrthreshold = 4)
sf_recurring200km4yr <- filter_occ_clust_yr(sf = sf_gbif1971_2020, clustcol = "Clust200km", yrthreshold = 4)


make_plots <- function(sf_pts, sf_cal, sf_base, sf_breeddist, outpng){
  png(outpng, width = 11.7, height = 8.3, units = "in", res = 300)
  plot(st_geometry(sf_cal), col = alpha("red", alpha = 0.4))
  patternLayer2(x = sf_cal, pattern = "diamond", density = 0.8, mode = "plot", add = T)
  plot(st_geometry(sf_base), add = T)
  plot(st_geometry(sf_breeddist), col = "green", add = T)
  plot(st_geometry(sf_pts), add = T)
  box()
  legendPattern(pos = "bottomright", 
                title.txt = NULL,
                categ = c("calibration area", "breeding \ndistribution"), 
                patterns = c("diamond","horizontal"), 
                col = c("black", "transparent"), 
                ptrn.bg = c(alpha("red", 0.4), "green"),
                cex = 1.1,
                text.cex = 1,
                values.cex = 1,
                frame = T)
  dev.off()
  
}

make_plots(sf_recurring32km4yr, sf_natcal, sf_poly, sf_starNat, "results/starling/occurrences/native_32km4yr_recurring.png")
make_plots(sf_recurring32km5yr, sf_natcal, sf_poly, sf_starNat, "results/starling/occurrences/native_32km5yr_recurring.png")
make_plots(sf_recurring100km4yr, sf_natcal, sf_poly, sf_starNat, "results/starling/occurrences/native_100km4yr_recurring.png")
make_plots(sf_recurring200km4yr, sf_natcal, sf_poly, sf_starNat, "results/starling/occurrences/native_200km4yr_recurring.png")

# Output to all recurring points to csv -----------------------------
fwrite(st_drop_geometry(sf_recurring32km4yr), "data/starling/filtered_occurrences/Starling_native_breeding_1971_2020_32km4yr_recurring.csv")
  

ls_inpoly <- st_intersects(sf_recurring32km4yr, sf_starNat)
v_inpoly = sapply(ls_inpoly, function(x){length(x)==0})
sf_not_in_poly <- sf_recurring32km4yr[v_inpoly,]
png("results/starling/occurrences/iberia_32km4yr_recurring_notinshp.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), ylim = c(33, 45), xlim = c(-11, 5))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_not_in_poly), add = T)
box()
dev.off()

png("results/starling/occurrences/SEurope_32km4yr_recurring_notinshp.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), ylim = c(33, 48), xlim = c(-11, 28))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_not_in_poly), add = T)
box()
dev.off()

png("results/starling/occurrences/SEurope_32km4yr_recurring.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), ylim = c(33, 48), xlim = c(-11, 28))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_recurring32km4yr), add = T)
box()
dev.off()

pts_PolygonCorner <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(-11, 44,
    -7.8, 43.8,      # allow clipping out some islands
    -3.4, 42.4,
    -1.5, 42,
    0, 40.4,
    2.1, 38.7,
    7.3, 38.7,
    7.3, 43.3,
    9.9, 43.3,
    12, 38,
    12, 30,
    -11, 30,
    -11, 44),
  ncol = 2, byrow = TRUE
)
sf_polyCorner <- st_polygon(x = list(pts_PolygonCorner)) %>%
  st_sfc(crs = st_crs(sf_poly))

pts_PolygonCorner2 <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(19, 27,
    19, 38.6,
    25, 38.6,
    25, 35.8,
    34.7, 35.8,
    37, 33,
    37, 27, 
    19, 27),
  ncol = 2, byrow = TRUE
)
sf_polyCorner2 <- st_polygon(x = list(pts_PolygonCorner2)) %>%
  st_sfc(crs = st_crs(sf_poly))

png("results/starling/occurrences/SEurope_32km4yr_recurring_notinshp_clippingarea.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), ylim = c(30, 48), xlim = c(-11, 28))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_recurring32km4yr), add = T)
plot(st_geometry(sf_polyCorner), col = alpha("red", 0.4), add = T)
box()
dev.off()


# Add S unicolor distribution 
sf_Sunicolor <- st_read(dsn = path.expand("data/starling/S_unicolor_distribution_map/data_0.shp"))
png("results/starling/occurrences/Med_32km4yr_recurring_notinshp_clippingarea.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly), ylim = c(30, 44), xlim = c(-11, 40))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_Sunicolor), add = T)
patternLayer2(x = sf_Sunicolor, pattern = "diamond", density = 0.8, add =T)
plot(st_geometry(sf_recurring32km4yr), add = T)
plot(st_geometry(sf_polyCorner), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_polyCorner2), col = alpha("red", 0.4), add = T)
box()
dev.off()

# Remove points from India not from the far NW and from China -------
# I could not find any records from these locations where they are 
# breeding. Records from UAE, however, is confirmed to be of 
# breeding birds.
#
# I also could not find any records of starlings breeding in
# 
pts_PolygonCorner3 <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(68, 21,
    80, 35,
    117, 44,
    135, 44,
    135, 5,
    68, 5,
    68, 21),
  ncol = 2, byrow = TRUE
)
sf_polyCorner3 <- st_polygon(x = list(pts_PolygonCorner3)) %>%
  st_sfc(crs = st_crs(sf_poly))

pts_PolygonCorner4 <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(-28.84, 72,
    145, 72,
    145, 81.85,
    -28.84, 81.85,
    -28.84, 72),
  ncol = 2, byrow = TRUE
)

sf_polyCorner4 <- st_polygon(x = list(pts_PolygonCorner4)) %>%
  st_sfc(crs = st_crs(sf_poly))

pts_kept <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(54, 24,
    57.5, 24,
    57.5, 27,
    54, 27,
    54, 24),
  ncol = 2, byrow = TRUE
)
sf_kept <- st_polygon(x = list(pts_kept)) %>%
  st_sfc(crs = st_crs(sf_poly))

sf_use_s2(FALSE)

sf_polycropped <- st_crop(sf_poly, xmin = -28.85, ymin = 0, xmax = 145, ymax = 81.85)
# Plot to show all the areas where the points are removed
png("results/starling/occurrences/Native_32km4yr_nonBreedPts2Remove.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_polycropped), axes = T, setParUsrBB = T, asp = 1)
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_recurring32km4yr), add = T)
plot(st_geometry(sf_polyCorner), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_polyCorner2), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_polyCorner3), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_polyCorner4), col = alpha("red", 0.4), add = T)
# points kept
plot(st_geometry(sf_kept), col = alpha("blue", 0.4), add = T)
# Add labels
text(55, 76, substitute(paste(bold("A"))), cex = 2, col = "black") 
text(5, 33, substitute(paste(bold("B"))), cex = 2, col = "black") 
text(22, 29, substitute(paste(bold("C"))), cex = 2, col = "black") 
text(110, 30, substitute(paste(bold("D"))), cex = 2, col = "black") 
text(55, 22, substitute(paste(bold("E"))), cex = 2, col = "black") 
box()
legend(x = -20, y = 17, 
       legend = c("Breeding range", "Points removed", "Points kept"), 
       fill = c("green", alpha("red", 0.4), alpha("blue", 0.4)),
       cex = 1.5)
legend(x = -20, y = 25,
       legend = c("Occurrences"),
       pch = 1,
       cex = 1.5)
dev.off()

# Filter out non-breeding localities --------------------------------
# Filter out points which fall within sf_polyCorner and sf_polyCorner2
ls_inpoly1 <- st_intersects(sf_recurring32km4yr, sf_polyCorner)
v_inpoly1 = sapply(ls_inpoly1, function(x){length(x)==0})
sf_recurring32km4yr <- sf_recurring32km4yr[v_inpoly1,]

ls_inpoly2 <- st_intersects(sf_recurring32km4yr, sf_polyCorner2)
v_inpoly2 = sapply(ls_inpoly2, function(x){length(x)==0})
sf_recurring32km4yr <- sf_recurring32km4yr[v_inpoly2,]

ls_inpoly3 <- st_intersects(sf_recurring32km4yr, sf_polyCorner3)
v_inpoly3 = sapply(ls_inpoly3, function(x){length(x)==0})
sf_recurring32km4yr <- sf_recurring32km4yr[v_inpoly3,]

sf_recurring32km4yr <- sf_recurring32km4yr %>% 
  filter(decimalLatitude < 72)
# Plot to show all the areas where the points are removed
png("results/starling/occurrences/Native_32km4yr_nonBreedPtsRemoved.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_starNat), col = "green", add = T)
plot(st_geometry(sf_recurring32km4yr), add = T)
box()
dev.off()

# Output breeding localities ----------------------------------------
fwrite(st_drop_geometry(sf_recurring32km4yr), "data/starling/filtered_occurrences/Starling_native_breeding_1971_2020_32km4yr_recurring_cleaned.csv")

# Retain only one point per raster grid cell ------------------------
# This is to reduce memory use because there are issues with RAM when thinning
#
r_cropped <- raster::raster("data/starling/native_calibration_area/bio1_cropped_masked.tif")
# Extract cell number to only get coordinates of cell midpoint to retain
cellNumber <- raster::extract(r_cropped,
                              sf_recurring32km4yr[,c("decimalLongitude", "decimalLatitude")],
                              cellnumbers = T)

df_recurringCellNumber <- cbind(st_drop_geometry(sf_recurring32km4yr) , cellNumber = cellNumber[,1])

#
# In the myna pipeline, the points were grouped by a cluster defined with
# distance larger than the home range. This is to account for rounding  
# of coordinates to the cell midpoint, which might put clusters slightly 
# closer together. With the myna data, the home range is 16km, and I used
# 20km cluster. 
#
# In the starling, the home range is 32km, and I use 32km cluster to group
# the points together. This is because the amount of data from the native range 
# is very large and this might reach a memory limit when thinning. I am happy to 
# compromise some points being slightly closer than 32km due to rounding 
# of coordinates. 
#
# Compare results/starling/occurrences/native_32km_cluster.png to
# results/starling/occurrences/native_40km_cluster.png
df_recurringCellSummary <- df_recurringCellNumber %>%
  dplyr::group_by(cellNumber) %>%
  dplyr::summarise(ClusterNumber = unique(Clust32km)[1],
                   ClusterTotal = length(unique(Clust32km)))

df_cellWithStarling <- data.frame(xyFromCell(object = r_cropped,
                                         cell = df_recurringCellSummary$cellNumber),
                              species = "Sturnus vulgaris",
                              Clust32km = df_recurringCellSummary$ClusterNumber)

fwrite(df_cellWithStarling, file = "data/starling/filtered_occurrences/Starling_native_breeding_1971-2020_32km4yr_recurring_cellwithStarling.csv")
