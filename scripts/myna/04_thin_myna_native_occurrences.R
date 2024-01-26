# 03_thin_myna_native_occurrences.R
# Load prerequisite libraries ---------------------------------------#####
library(sf)
# library(rgdal)  # readOGR/writeOGR function to read/write shapefils
library(tidyverse)  # data manipulation
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset. 
# Read in occurrences -----------------------------------------------#####
df_gbif1971_2020v2 <- fread("data/myna/filtered_occurrences/gbif_native_1971-2020_with_cluster.csv")

sf_gbif1971_2020 <- st_as_sf(df_gbif1971_2020v2, coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)

# Read this in for potential base map for plotting occurrence points
sf_shorelinesclipped <- st_read(dsn= path.expand("data/myna/native_plot_area/native_plot_area.shp"))
sf_polyClip <- st_read(dsn= path.expand("data/myna/native_calibration_area/native_calibration_area.shp"))
sf_coastline <- st_read(dsn = path.expand("data/ne_10m_coastline/ne_10m_coastline.shp"))
sf_country <- st_read(dsn = path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))

# Plot native points by defined localities ----------------------------#####
# png("results/myna/occurrences/native_10km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_polyClip)) 
# plot(st_geometry(sf_country), add = T)
# points(sf_gbif1971_2020, col = factor(sf_gbif1971_2020$Clust10km))
# dev.off()
#
# png("results/myna/occurrences/native_16km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_polyClip), col = alpha("grey", 0.2))
# plot(st_geometry(sf_country), add = T)
# plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust16km), add = T)
# dev.off()
#
# png("results/myna/occurrences/native_20km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_polyClip), col = alpha("grey", 0.2))
# plot(st_geometry(sf_country), add = T)
# plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust20km), add = T)
# dev.off()
#
# png("results/myna/occurrences/native_100km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
# plot(st_geometry(sf_polyClip)) 
# plot(st_geometry(sf_country), add = T)
# points(sf_gbif1971_2020, col = factor(sf_gbif1971_2020$Clust100km))
# dev.off()
# 
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

# Define a recurring localities using the 4 year mark as the common myna
# average lifespan is about 4 years
df_recurring16km <- df_sum16km[yr_range > 4,]

# Subset for the recurring localities based on the cluster that passed
# the filter.
sf_recurring16km <- sf_gbif1971_2020[sf_gbif1971_2020$Clust16km %in% df_recurring16km$Clust16km, ]

# No longer used: Remove outlier localities in China ----------------#####
# This is no longer needed but kept here for potential use in the future
# Check some potential outlier localities in China 
# df_out <- df_sum16km %>%
#   filter(avg_y > 22) %>%
#   filter(avg_x > 102) %>%
#   filter(yr_range > 4)
# # Remove Kunming and Chongqing locality 
# sf_recurring16km <- sf_recurring16km[!(sf_recurring16km$decimalLatitude > 22 &
#                                        sf_recurring16km$decimalLongitude > 102 &
#                                        sf_recurring16km$decimalLongitude < 110),]

png("results/myna/occurrences/native_recurring_16km_cluster_5yr.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(sf_shorelinesclipped)
plot(st_geometry(sf_polyClip), col = "green", border = "green", xlim = c(45, 124), ylim = c(0, 55))
# plot(st_geometry(sf_shorelinesclipped), xlim = c(45, 124), ylim = c(0, 55), add = T)
plot(st_geometry(sf_country), add = T)
plot(st_geometry(sf_recurring16km), col = "black", add = T)
dev.off()

# Thin dataset ------------------------------------------------------#####
#
# There are a lot of issues with RAM when doing the spatial thinning
#
#
# Only retain one point per raster grid cell to reduce amount of
# memory used
r_cropped <- raster::raster("data/myna/native_calibration_area/bio1_cropped_masked.tif")
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

fwrite(df_cellWithMyna, file = "data/myna/filtered_occurrences/gbif_native_1971-2020_16km_recurring_cellwithmyna.csv")
# df_cellWithMyna <- fread(file = "data/myna/filtered_occurrences/gbif_native_1971-2020_16km_recurring_cellwithmyna.csv")

# Quick count of number of points per 20 km cluster in case we need
# to thin by cluster to reduce RAM problems
df_sum20kmCluster <- df_cellWithMyna %>%
  dplyr::group_by(Clust20km) %>%
  dplyr::count() 

max(df_sum20kmCluster$n)
df_sum20kmCluster_n1 <- df_sum20kmCluster %>% filter(n == 1)

# Thin by cluster defined based on 20km cluster defined earlier
for (clust in unique(df_cellWithMyna$Clust20km)){
  nkm <- 16
  outFile <- paste("myna_native_16kmthinned_clust", sprintf("%04d", clust), sep = "")
  print(outFile)
  df_subset <- df_cellWithMyna[df_cellWithMyna$Clust20km == clust,]
  nrep <- nrow(df_subset)
  if (nrep == 1){
    write_csv(df_subset[,c("species", "x", "y")], 
              file = paste("data/myna/filtered_occurrences/native_thinned/16km/", outFile, "_thin1.csv", sep = ""),
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
                 out.dir = "data/myna/filtered_occurrences/native_thinned/16km",
                 out.base = outFile,
                 write.log.file = F)
  }
}


# Thin based on rounded coordinates to 0.05 decimal degrees first
# This is because there are too many points to thin and this keeps
# resulting in too many points
# df_cellWithMyna1 <- df_cellWithMyna %>%
#   dplyr::mutate(decimalLongitude_1 = plyr::round_any(x, accuracy = 0.1, f = round)) %>%
#   dplyr::mutate(decimalLatitude_1 = plyr::round_any(y, accuracy = 0.1, f = round)) %>%
#   dplyr::mutate(decimalLongitude_15 = plyr::round_any(x, accuracy = 0.15, f = round)) %>%
#   dplyr::mutate(decimalLatitude_15 = plyr::round_any(y, accuracy = 0.15, f = round)) %>%
#   dplyr::mutate(decimalLongitude_05 = plyr::round_any(x, accuracy = 0.05, f = round)) %>%
#   dplyr::mutate(decimalLatitude_05 = plyr::round_any(y, accuracy = 0.05, f = round))
# # Randomly retaining one record every 0.05 decimal degrees
# df_cellWithMyna_05 <- data.table(df_cellWithMyna1)[, .SD[sample(.N,1)], 
#                                                    keyby = .(decimalLongitude_05 , decimalLatitude_05)]
# 
# df_sum20kmCluster_05 <- df_cellWithMyna_05 %>%
#   dplyr::group_by(Clust20km) %>%
#   dplyr::count() 
# 
# max(df_sum20kmCluster_05$n)
# 
# # Thin by cluster defined based on 20km cluster defined earlier
# # Use thinned dataset already thinned to retain one record for 
# # every 0.05 cell.
# for (clust in unique(df_cellWithMyna_05$Clust20km)){
#   nkm <- 16
#   outFile <- paste("myna_native_16kmthinned_clust", sprintf("%04d", clust), sep = "")
#   print(outFile)
#   df_subset <- df_cellWithMyna_05[df_cellWithMyna_05$Clust20km == clust,]
#   nrep <- nrow(df_subset)
#   if (nrep == 1){
#     write.csv(df_subset[,c("species", "x", "y")], 
#               file = paste("data/filtered_occurrences/native_thinned/16km_mod/", outFile, "_thin1.csv", sep = ""),
#               row.names = F)
#   } else {
#     spThin::thin(df_cellWithMyna_05[df_cellWithMyna_05$Clust20km == clust,],
#                  lat.col = "y",
#                  long.col = "x",
#                  spec.col = "species",
#                  thin.par = nkm,  # 16km as this is the supposed home range
#                  reps = 25,
#                  locs.thinned.list.return = F,  # False so that the records are written into file directly
#                  write.files = TRUE,
#                  max.files = 1,
#                  out.dir = "data/filtered_occurrences/native_thinned/16km_mod/",
#                  out.base = outFile,
#                  write.log.file = F)
#   }
# }
