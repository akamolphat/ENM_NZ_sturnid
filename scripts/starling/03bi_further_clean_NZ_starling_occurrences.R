# 03bi_further_clean_NZ_starling_occurrences.R
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
sf_NZcal <- st_read(dsn = "data/starling/NZ_calibration_area/starling_NZ_calibration_area.shp")
sf_poly <- st_read(dsn = path.expand("data/starling/NZ_calibration_area/starling_NZ_calibration_area.shp"))
sf_gbif1971_2020 <- fread("data/starling/filtered_occurrences/Starling_NZ_1971_2020_withclust.csv") %>%
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)
st_crs(sf_gbif1971_2020) <- "WGS84"  # or 4326
# Plot NZ points by defined localities ----------------------------#####
dir.create("results/starling/occurrences", recursive = T, showWarnings = F)
png("results/starling/occurrences/NZ_10km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust10km), add = T)
dev.off()

png("results/starling/occurrences/NZ_32km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust32km), add = T)
dev.off()

png("results/starling/occurrences/NZ_50km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust50km), add = T)
dev.off()

png("results/starling/occurrences/NZ_100km_cluster.png", width = 11.7, height = 8.3, units = "in", res = 300)
plot(st_geometry(sf_poly))
plot(st_geometry(sf_gbif1971_2020), col = factor(sf_gbif1971_2020$Clust100km), add = T)
dev.off()
# Filter for occurrences from recurring locality --------------------

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

# Output breeding localities ----------------------------------------
fwrite(st_drop_geometry(sf_recurring32km4yr), "data/starling/filtered_occurrences/Starling_NZ_breeding_1971_2020_32km4yr_recurring.csv")

# Retain only one point per raster grid cell ------------------------
# This is to reduce memory use because there are issues with RAM when thinning
#
r_cropped <- raster::raster("data/starling/NZ_calibration_area/bio1_cropped_masked.tif")
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
#
df_recurringCellSummary <- df_recurringCellNumber %>%
  dplyr::group_by(cellNumber) %>%
  dplyr::summarise(ClusterNumber = unique(Clust32km)[1],
                   ClusterTotal = length(unique(Clust32km)))

df_cellWithStarling <- data.frame(xyFromCell(object = r_cropped,
                                             cell = df_recurringCellSummary$cellNumber),
                                  species = "Sturnus vulgaris",
                                  Clust32km = df_recurringCellSummary$ClusterNumber)

fwrite(df_cellWithStarling, file = "data/starling/filtered_occurrences/Starling_NZ_breeding_1971-2020_32km4yr_recurring_cellwithStarling.csv")
