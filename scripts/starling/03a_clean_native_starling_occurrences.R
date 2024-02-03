# 03a_clean_native_starling_occurrence_data.R -------------------------------#####
#
# This script clean native starling occurrence data.
#
# Standard cleaning of occurrences followed the guidelines in Cobos et al. (2018)
# http://dx.doi.org/10.17161/bi.v13i0.7600
#
# The order was altered slightly. Some filters were not necessary and
# some additional filters were added.
#
# The order I do my filters are as follows:
#
# 1. Partition data to native and NZ range. This is to reduce the amount 
# of memory to process
# 1. Remove missing data
# 2. Remove (0,0) coordinates
# 3. Remove repeated observations
# 4. Remove records with low decimal precision
# 5. Errors in scientific names (NOT APPLICABLE)
# 6. Remove records with likely swapped latitude-longitude (NOT REQUIRED)
# 7. Remove records outside of continents
# 8. Remove records outside the time of interest
#
# Additional filters to retain points in recurring localities
# 
# We must first define the recurring localities. It is best done at a lower
# resolution to reduce the amount of memory required to calculate pairwise
# distances between points. We can then assign the defined "clusters" to the
# high precision coordinates based on the rounded coordinates This is assuming
# that the distances you are working with to define "localities" are within the
# error the rounding of the coordinates. For example, I rounded the coordinates 
# to 0.05 decimal degrees, but I define localities to be recurring points within
# 16km.  
# 9. Remove records outside the region of interest
# 10. Only retain points in recurring localities

# 9. Environmental outliers
#
# Load libraries and functions --------------------------------------#####
library(rgdal)  # readOGR/writeOGR function to read/write shapefils
library(sf)
library(tidyverse)  # data manipulation
# library(plotly)  # some interactive plots for some basic visualisation
# library(rgeos)  # gUnaryUnion and gIntersect function for merging/clipping
library(data.table)  # used for reading in file (faster and more memory efficient)
library(raster)  # for reading tiff file
library(rasterVis)  # for quick visualisation of tiff file
library(spThin)  # for thinning dataset. 
# library(humboldt)  # for thinning dataset.

decimalplaces <- function(x) {
  # This function counts the number of decimal places
  # This function should work with numeric, or with a vector of numbers
  ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
         nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
         0)
}

# Read coastline shapefile -------------------------------------------#####
# Read this in for potential base map for plotting occurence points
sf_coastline <- st_read(dsn=path.expand("data/ne_10m_coastline/ne_10m_coastline.shp"))
sf_admin <- st_read(dsn=path.expand("data/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp"))
plot(st_geometry(sf_admin))
sf_coastline_dfpolygons <- as(sf_admin, "Spatial") # If you want sp
sf_cal <- st_read(dsn = path.expand("data/starling/native_calibration_area/starling_native_calibration_area.shp"))

# # Read occurrences data ----------------------------------------------#####
# # fread function from data.table function is used as the csv file is 
# # large and it is much faster at reading large files.
# df_gbif <- fread("data/starling/raw_occurrences/0423711-210914110416597.csv", quote = "", encoding = "UTF-8")
# 
# # Creates a list of issues, so we know what we can filter based on
# unique(unlist(str_split(unique(df_gbif$issue), ";")))
# 
# # Start filtering ---------------------------------------------------#####
# df_gbif <- df_gbif %>% 
#   ## 1. Remove missing data -----------------------------------------#####
#   drop_na(decimalLongitude, decimalLatitude) %>%  # remove records with missing coordinates
#   drop_na(eventDate) %>%  # remove records with missing dates
#   ## Remove some invalid dates as it might mess up with our record filtering
#   dplyr::filter(!grepl("RECORDED_DATE_INVALID", issue)) %>%
#   ## 2. Remove (0,0) coordinates ------------------------------------#####
#   dplyr::filter(!grepl("ZERO_COORDINATE", issue)) %>% 
#   ## 8. Remove records outside of time period of interest --------------#####
#   dplyr::filter(year <= 2020 & year >= 1971) %>%
#   dplyr::filter(occurrenceStatus == "PRESENT") 
# gc()
# 
# # Partition data to only native range -------------------------------#####
# df_nat <- df_gbif %>%
#   filter(decimalLongitude > -40 & decimalLatitude > 0)
# fwrite(df_nat, "data/starling/filtered_occurrences/Starling_native_1971_2020.csv")
# 
# # Partition data to only NZ -----------------------------------------#####
# df_NZ <- df_gbif %>%
#   filter(decimalLongitude > 164 & decimalLatitude < - 33)
# fwrite(df_NZ, "data/starling/filtered_occurrences/Starling_NZ_1971_2020.csv")
# 
# rm(df_gbif)
# gc()

df_nat <- fread("data/starling/filtered_occurrences/Starling_native_1971_2020.csv")

# 3. Remove repeated observations -----------------------------------#####
# Remove observations with the same coordinates, coordinate uncertainty
# and day of observation.
df_nat <- unique(df_nat[,c("species", "occurrenceStatus", "countryCode",
                                  "decimalLatitude", "decimalLongitude", 
                                  "coordinateUncertaintyInMeters", "day", 
                                  "month", "year")])

# 4. Remove records with low coordinate precision -------------------#####
# Used the cut-off point at 4 decimal places. At 4 decimal places, 
# the precision is approximately 11m.
# 5 decimal places are at approximately 1.1m
# The code below also removes trailing zeros before performing this
# filter as it is unlikely that 
df_natHighPrec <- df_nat %>%
  dplyr::filter(decimalplaces(as.numeric(sub("0+$", "", as.character(decimalLongitude)))) > 3) %>%
  dplyr::filter(decimalplaces(as.numeric(sub("0+$", "", as.character(decimalLatitude)))) > 3) %>% 
  # filter for only precision <= 100m. Still include NA
  dplyr::filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
  dplyr::mutate(decimalLongitude3DP = round(decimalLongitude, digits = 3)) %>%
  dplyr::mutate(decimalLatitude3DP = round(decimalLatitude, digits = 3)) %>%
  dplyr::mutate(decimalLongitude2DP = round(decimalLongitude, digits = 2)) %>%
  dplyr::mutate(decimalLatitude2DP = round(decimalLatitude, digits = 2)) %>%
  dplyr::mutate(decimalLongitude_05 = plyr::round_any(decimalLongitude, accuracy = 0.05, f = round))%>%
  dplyr::mutate(decimalLatitude_05 = plyr::round_any(decimalLatitude, accuracy = 0.05, f = round))
rm(df_nat)
gc()

# 5. Errors in scientific names -------------------------------------#####
# This dataset was obtained based on the species name so no errors in 
# scientific names
# 6. Changes in latitude-longitude ----------------------------------#####
# Visual inspection of maps did not find obvious cases of swapped in 
# latitude and longitude
# 7. Records outside the continents ---------------------------------#####
sf_natHighPrec <- st_as_sf(df_natHighPrec, coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)
st_crs(sf_natHighPrec) <- "WGS84"  # or 4326
sf::sf_use_s2(FALSE)
sf_natInPoly <- st_filter(sf_natHighPrec, sf_admin)  # Filter for only points in polygon

# png("results/starling/native_occ.png")
# plot(st_geometry(sf_coastline), xlim = c(-40,180), ylim = c(0, 90))
# plot(st_geometry(df_natHighPrec), add = T)
# dev.off()
# 
# png("results/starling/native_occ_in_poly.png")
# plot(st_geometry(sf_coastline), xlim = c(-40,180), ylim = c(0, 90))
# plot(st_geometry(sf_natInPoly), add = T)
# dev.off()

# sf_natInPoly$date <- as.Date(with(sf_natInPoly, paste(year, month, day,sep="-")), "%Y-%m-%d")
# sf_natBreed <- sf_natInPoly %>% 
#   filter(month >= 3 & month <= 7) %>% 
#   filter((month >= 3 & day >= 15) | (month > 3)) %>%
#   filter((month <= 7 & day < 15) | (month < 7))
# range(sf_natBreed$date, na.rm = T)
# 
# sf_natnonBreed <- sf_natInPoly %>% 
#   filter((month == 3 & day < 15) | (month > 7) | (month < 3) | (month == 7 & day >= 15) )
# range(sf_natnonBreed$date, na.rm = T)

sf_natBreed <- sf_natInPoly %>%
  filter(month >= 4 & month <= 6)
sf_natnonBreed <- sf_natInPoly %>%
  filter((month >= 7) | (month <= 3))

png("results/starling/native_occ_breeding.png")
plot(st_geometry(sf_coastline), xlim = c(-40,180), ylim = c(0, 90))
plot(st_geometry(sf_natBreed), add = T)
dev.off()

png("results/starling/native_occ_nonbreeding.png")
plot(st_geometry(sf_coastline), xlim = c(-40,180), ylim = c(0, 90))
plot(st_geometry(sf_natnonBreed), add = T)
dev.off()

# 9. Records outside the region of interest -------------------------#####
# Remove one record from Kenya which is obviously a rare vagrant that is 
# not breeding. This will obviously be removed with subsequent filters but
# will likely use more memory when memory is already a limiting factor
#
# There will be more localities removed once recurring localities
# are defined but that will depend on how recurring localities are defined
# (distance and number of recurring years)
#
sf_natBreed <- sf_natBreed %>%
  filter(decimalLatitude > 1)

sf::sf_use_s2(FALSE)
# Remove records from outside calibration area
sf_natBreed <- st_filter(sf_natBreed, sf_cal)  # Filter for only points in polygon


# 9.a.i. Define localities ------------------------------------------#####
# 8. Retain only records with min/max yr when rounded to 0.05 
#
# Only store the min and max year of record from the particular location at 0.05 decimal degrees
sf_minYr <- setDT(sf_natBreed)[ , .SD[which.min(year)], by = list(decimalLatitude_05, decimalLongitude_05)]  # return record with min year
sf_maxYr <- setDT(sf_natBreed)[ , .SD[which.max(year)], by = list(decimalLatitude_05, decimalLongitude_05)]  # return record with max year
sf_minMaxYr <- rbind(sf_minYr, sf_maxYr) %>% distinct() # remove repeated records (cases where min = max year)

# Create a table of just the unique coordinates for performing distance based clustering
sf_minMaxYrUnique <- sf_minMaxYr %>%
  dplyr::select(species, decimalLongitude_05, decimalLatitude_05) %>%
  distinct()
  
# # Create sf object to create distance matrix between points
# sf_natBreedUnique <- st_as_sf(sf_minMaxYrUnique, coords = c("decimalLongitude_05","decimalLatitude_05"), remove = FALSE)
# st_crs(sf_natBreedUnique) <- "WGS84"  # or 4326

# Convert decimal degree coordinates in WGS1984 to Mercator as coordinates are in 
# metres
coords <- dismo::Mercator(sf_minMaxYrUnique[,2:3])
# Calculate distance matr
distmat <- dist(data.frame(rownames=rownames(sf_minMaxYrUnique), 
                           x=coords[,1],
                           y=coords[,2]))  # euclidean distances between points in meters

chc <- hclust(distmat, method="single")  # single method adopts a friend of friend approach

# Distance cluster with a 10km, 20km, 30km, 50km, 100km, 120km, 150km, 200km threshold  
chc.d10 <- cutree(chc, h=10000) 
chc.d20 <- cutree(chc, h=20000) 
chc.d30 <- cutree(chc, h=30000) 
chc.d32 <- cutree(chc, h=32000) 
chc.d40 <- cutree(chc, h=40000)
chc.d50 <- cutree(chc, h=50000) 
chc.d100 <- cutree(chc, h=100000) 
chc.d120 <- cutree(chc, h=120000) 
chc.d150 <- cutree(chc, h=150000) 
chc.d200 <- cutree(chc, h=200000) 

# Join results to points
sf_minMaxYrUnique <- data.table(sf_minMaxYrUnique, 
                                Clust10km=chc.d10, 
                                Clust20km=chc.d20, 
                                Clust30km=chc.d30, 
                                Clust32km=chc.d32, 
                                Clust40km=chc.d40, 
                                Clust50km=chc.d50, 
                                Clust100km=chc.d100, 
                                Clust120km=chc.d120, 
                                Clust150km=chc.d150, 
                                Clust200km=chc.d200)

# A few plots to see different thresholds
p <- ggplot() + geom_sf(data = sf_coastline) +
  coord_sf(xlim = c(-40, 180), ylim = c(0, 90)) + theme(legend.position = "none")
png("results/starling/breeding_localities_def/10km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust10km)))
dev.off()

png("results/starling/breeding_localities_def/20km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust20km)))
dev.off()

png("results/starling/breeding_localities_def/32km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust32km)))
dev.off()

png("results/starling/breeding_localities_def/40km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust50km)))
dev.off()

png("results/starling/breeding_localities_def/50km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust50km)))
dev.off()

png("results/starling/breeding_localities_def/100km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust100km)))
dev.off()

png("results/starling/breeding_localities_def/200km_cluster.png")
p + geom_point(data = sf_minMaxYrUnique,
               aes(x = decimalLongitude_05, y = decimalLatitude_05, col = factor(Clust200km)))
dev.off()

df_gbif1971_2020v2 <- merge(x = sf_natBreed, 
                            y = sf_minMaxYrUnique, 
                            by = c("species", "decimalLatitude_05", "decimalLongitude_05"))

# 9.a.ii. Output native filtered points with defined localities -----#####
fwrite(df_gbif1971_2020v2, file = "data/starling/filtered_occurrences/Starling_native_breeding_1971_2020_withclust.csv", quote = F)


