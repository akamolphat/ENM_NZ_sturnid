# 04_thin_NZ_occurrences.R
# Load prerequisite libraries ---------------------------------------#####
# library(rgdal)  # readOGR/writeOGR function to read/write shapefils
library(tidyverse)  # data manipulation
# library(plotly)  # some interactive plots for some basic visualisation
# library(rgeos)  # gUnaryUnion and gIntersect function for merging/clipping
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset. 
library(sf)

# Reads in NZ occurrence points -------------------------------------#####
df_gbif1971_2020 <- fread("data/myna/filtered_occurrences/gbif_NZ_1971-2020_with_cluster.csv")

sf_gbif1971_2020 <- st_as_sf(df_gbif1971_2020, coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)

sf_clippedRegion <- st_read(dsn= path.expand("data/myna/NZ_calibration_area/NZ_calibration_area.shp"))


# Plot NZ points by defined localities ------------------------------#####
# png("results/myna/occurrences/NZ_10km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(pts_gbif1971_2020v2, col = factor(pts_gbif1971_2020v2$Clust10km))
# dev.off()
# 
# png("results/myna/occurrences/NZ_16km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(pts_gbif1971_2020v2, col = factor(pts_gbif1971_2020v2$Clust16km))
# dev.off()
# 
# png("results/myna/occurrences/NZ_20km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(pts_gbif1971_2020v2, col = factor(pts_gbif1971_2020v2$Clust20km))
# dev.off()
# 
# png("results/myna/occurrences/NZ_100km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(pts_gbif1971_2020v2, col = factor(pts_gbif1971_2020v2$Clust100km))
# dev.off()

# 9.a.ii. Filter for occurrences from recurring locality ------------#####
# 16km cluster is used as this is the estimated homerange of the 
# common myna
df_sum16km <- setDT(st_drop_geometry(sf_gbif1971_2020))[, .(
  avg_x = mean(decimalLongitude),
  avg_y = mean(decimalLatitude),
  min_yr = min(year),
  max_yr = max(year),
  n_occ = length(year),
  yr_range = diff(range(year))
), keyby = .(Clust16km)]
# Plot localities potentially outliers (south of -40 degrees) -------#####
# df_out <- df_sum16km %>% 
#   filter(avg_y < -40) %>%
#   filter(yr_range > 4)
# 
# plot(st_geometry(sf_clippedRegion))
# points(df_out$avg_x, df_out$avg_y, col = df_out$Clust16km)
# Define a recurring localities using the 4 year mark as the common myna
# average lifespan is about 4 years 
df_recurring16km <- df_sum16km[yr_range > 4,]
# Subset for the recurring localities based on the cluster that passed 
# the filter. 
sf_recurring16km <- sf_gbif1971_2020[sf_gbif1971_2020$Clust16km %in% df_recurring16km$Clust16km, ]
# Make NZ map to show which point were removed ----------------------
pts_PolygonCorner1 <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(166, -42,
    174.5, -42,
    174.5, -46.9,
    166, -46.9,
    166, -42),
  ncol = 2, byrow = TRUE
)

sf_polyCorner1 <- st_polygon(x = list(pts_PolygonCorner1)) %>%
  st_sfc(crs = st_crs(sf_clippedRegion))
# Points reviewed but kept 
pts_PolygonCorner2 <- matrix(  # Polygon has to be a pentagon as a rectangle does not 
  c(174.5, -40.3,
    176, -40.3,
    176, -41.7,
    174.5, -41.7,
    174.5, -40.3),
  ncol = 2, byrow = TRUE
)

sf_polyCorner2 <- st_polygon(x = list(pts_PolygonCorner2)) %>%
  st_sfc(crs = st_crs(sf_clippedRegion))


sf_use_s2(FALSE)

st_nat <- st_read("data/myna/A_tristis_distribution_map/data_0.shp")
# Plot to show all the areas where the points are removed
png("results/myna/occurrences/NZ_16km5yr_Outlier2Remove.png", width = 6.5, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_clippedRegion), axes = T)
plot(st_geometry(st_nat), col = "green", add = T)
plot(st_geometry(sf_recurring16km), add = T)
plot(st_geometry(sf_polyCorner1), col = alpha("red", 0.4), add = T)
plot(st_geometry(sf_polyCorner2), col = alpha("blue", 0.4), add = T)
text(173.5, -44, substitute(paste(bold("A"))), cex = 2, col = "black") 
text(176, -42, substitute(paste(bold("B"))), cex = 2, col = "black") 
box()
legend(x = 166, y = -35,
       legend = c("Breeding range", "Points removed", "Points kept"),
       fill = c("green", alpha("red", 0.4), alpha("blue", 0.4)),
       cex = 1.5)
legend(x = 166, y = -37.8,
       legend = c("Occurrences"),
       pch = 1,
       cex = 1.5)
dev.off()

# Remove Christchurch locality --------------------------------------#####
sf_recurring16km <- sf_recurring16km[sf_recurring16km$decimalLatitude > -42,]
# Check out each cluster at latitude < -40 
# pts_out <- sf_recurring16km[sf_recurring16km$Clust16km %in% df_out$Clust16km, ]
# df_recurring16km <- df_sum16km[yr_range > 4,]
# sf_recurring16km <- pts_gbif1971_2020v2[pts_gbif1971_2020v2$Clust16km %in% df_recurring16km$Clust16km, ]
# png("results/GBIF_maps/NZ_recurring_16km_cluster_5yr.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(sf_recurring16km)
# dev.off()
# 
# df_recurring16km <- df_sum16km[yr_range > 5,]
# sf_recurring16km <- pts_gbif1971_2020v2[pts_gbif1971_2020v2$Clust16km %in% df_recurring16km$Clust16km, ]
# png("results/GBIF_maps/NZ_recurring_16km_cluster_6yr.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(sf_recurring16km)
# dev.off()
# 
# df_recurring16km <- df_sum16km[yr_range >= 10,]
# sf_recurring16km <- pts_gbif1971_2020v2[pts_gbif1971_2020v2$Clust16km %in% df_recurring16km$Clust16km, ]
# png("results/GBIF_maps/NZ_recurring_16km_cluster_10yr.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_clippedRegion))
# points(sf_recurring16km)
# dev.off()

# Thin dataset ------------------------------------------------------#####
#
# There are a lot of issues with RAM when doing the spatial thinning
#
#
# Only retain one point per raster grid cell to reduce amount of 
# memory used
r_cropped <- raster::raster("data/myna/NZ_calibration_area/bio1_cropped_masked.tif")
# Extract cell number to only get coordinates of cell midpoint to retain
cellNumber <- raster::extract(r_cropped,
                              sf_recurring16km[,c("decimalLongitude", "decimalLatitude")],
                              cellnumbers = T)

df_recurring16kmCellNumber <- cbind(st_drop_geometry(sf_recurring16km) , cellNumber = cellNumber[,1])
df_recurring16kmCellSummary <- df_recurring16kmCellNumber %>%
  dplyr::group_by(cellNumber) %>%
  dplyr::summarise(Cluster20kmNumber = unique(Clust20km)[1],
                   Cluster20kmtotal = length(unique(Clust20km)))

df_cellWithMyna <- data.frame(xyFromCell(object = r_cropped, 
                                         cell = df_recurring16kmCellSummary$cellNumber), 
                              species = "Acridotheres tristis",
                              Clust20km = df_recurring16kmCellSummary$Cluster20kmNumber)

fwrite(df_cellWithMyna, file = "data/myna/filtered_occurrences/gbif_NZ_1971-2020_16km_recurring_cellwithmyna.csv")

# Quick count of number of points per 20 km cluster in case we need
# to thin by cluster to reduce RAM problems
df_sum20kmCluster <- df_cellWithMyna %>%
  group_by(Clust20km) %>%
  count() 

max(df_sum20kmCluster$n)
df_sum20kmCluster_n1 <- df_sum20kmCluster %>% filter(n == 1)

# Thin by cluster
for (clust in unique(df_cellWithMyna$Clust20km)){
  nkm <- 16
  outFile <- paste("myna_NZ_16kmthinned_clust", sprintf("%04d", clust), sep = "")
  print(outFile)
  df_subset <- df_cellWithMyna[df_cellWithMyna$Clust20km == clust,]
  nrep <- nrow(df_subset)
  if (nrep == 1){
    write_csv(df_subset[,c("species", "x", "y")], 
              file = paste("data/myna/filtered_occurrences/NZ_thinned/16km/", outFile, "_thin1.csv", sep = ""),
              quote = "none")
  } else {
    spThin::thin(df_cellWithMyna[df_cellWithMyna$Clust20km == clust,],
                 lat.col = "y",
                 long.col = "x",
                 spec.col = "species",
                 thin.par = nkm,  # 16km as this is the supposed home range
                 reps = 25,
                 locs.thinned.list.return = F,  # False so that the records are written into file directly
                 write.files = TRUE,
                 max.files = 1,
                 out.dir = "data/myna/filtered_occurrences/NZ_thinned/16km",
                 out.base = outFile,
                 write.log.file = F)
  }
}
