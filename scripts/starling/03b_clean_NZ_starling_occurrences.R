# 03b_clean_starling_NZ_occurrences.R
#
# NZ ONLY!!!!
#
# This script clean occurrences data, reduce the dataset to only the NZ range
# and also performed spatial thinning in order to reduce the dataset to only the relavent points in the 
#
# Standard cleaning of occurrences followed the guidelines in Cobos et al. (2018)
# http://dx.doi.org/10.17161/bi.v13i0.7600
#
# The order was altered slightly. Some filters were not necessary and
# some additional filters were added
#
# The order I do my filters are as follows:
#
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
library(sf)
library(tidyverse)  # data manipulation
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
# Read this in for potential base map for plotting occurrence points
sf_NZshp <- st_read(dsn= path.expand("data/starling/NZ_calibration_area/starling_NZ_calibration_area.shp"))

# Read occurrences data ----------------------------------------------#####
# fread function from data.table function is used as the csv file is 
# large and it is much faster at reading large files.
df_gbif <- fread("data/starling/raw_occurrences/0423711-210914110416597.csv", quote = "", encoding = "UTF-8")
# Subset dataset for only the NZ range ------------------------------#####
df_gbif <- df_gbif %>%
  filter(countryCode == "NZ")

# Creates a list of issues, so we know what we can filter based on
unique(unlist(str_split(unique(df_gbif$issue), ";")))
# Start filtering ---------------------------------------------------#####
df_gbif <- df_gbif %>% 
  ## 1. Remove missing data -----------------------------------------#####
drop_na(decimalLongitude, decimalLatitude) %>%  # remove records with missing coordinates
  drop_na(eventDate) %>%  # remove records with missing dates
  ## Remove some invalid dates as it might mess up with our record filtering
  dplyr::filter(!grepl("RECORDED_DATE_INVALID", issue)) %>%
  ## 2. Remove (0,0) coordinates ------------------------------------#####
dplyr::filter(!grepl("ZERO_COORDINATE", issue))
# 3. Remove repeated observations -----------------------------------#####
# Remove observations with the same coordinates, coordinate uncertainty
# and day of observation.
df_gbifNoRep <- unique(df_gbif[,c("species", "occurrenceStatus", "countryCode",
                                  "decimalLatitude", "decimalLongitude", 
                                  "coordinateUncertaintyInMeters", "day", 
                                  "month", "year")])

# 4. Remove records with low coordinate precision -------------------#####
# Used the cut-off point at 4 decimal places. At 4 decimal places, 
# the precision is approximately 11m.
# 5 decimal places are at approximately 1.1m
# The code below also removes trailing zeros before performing this
# filter as it is unlikely that 
df_gbifHighPrec <- df_gbifNoRep %>%
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


# 5. Errors in scientific names -------------------------------------#####
# This dataset was obtained based on the species name so no errors in 
# scientific names
# 6. Changes in latitude-longitude ----------------------------------#####
# Visual inspection of maps did not find obvious cases of swapped in 
# latitude and longitude
# 7. Records outside the continents ---------------------------------#####
# Visual inspection of maps did not find any records in the middle of the
# sea that is not an island.

# 8. Remove records outside of time period of interest --------------#####
df_gbif1971_2020 <- df_gbifHighPrec %>% 
  dplyr::filter(year <= 2020 & year >= 1971) %>%
  dplyr::filter(occurrenceStatus == "PRESENT") 
rm(df_gbif)
gc()


# 9. Records outside the region of interest -------------------------#####
# 9.a. NZ area ------------------------------------------------------#####
sf_gbif1971_2020 <- st_as_sf(df_gbif1971_2020, coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)
st_crs(sf_gbif1971_2020) <- "WGS84"  # or 4326
sf::sf_use_s2(FALSE)
sf_NZInPoly <- st_filter(sf_gbif1971_2020, sf_NZshp) 
# Make quick plot
png("results/starling/occurrences/gbif_NZ_range_1971-2020.png", width = 11.7, height = 8.3, res = 300, units = "in")
plot(st_geometry(sf_NZshp), col = "green", border = "green", xlim = c(45, 124), ylim = c(0, 55))
plot(st_geometry(sf_NZshp), xlim = c(45, 124), ylim = c(0, 55), add = T)
points(x = df_gbif1971_2020$decimalLongitude,
       y = df_gbif1971_2020$decimalLatitude, col = "red")
plot(st_geometry(sf_NZInPoly), col = "blue", add = T)
dev.off()


# 9.a.i. Define localities ------------------------------------------#####
# 8. Retain only records with min/max yr when rounded to 0.05 
#
df_NZInPoly <- st_drop_geometry(sf_NZInPoly)
# Only store the min and max year of record from the particular location at 0.05 decimal degrees
df_minYr <- setDT(df_NZInPoly)[ , .SD[which.min(year)], by = list(decimalLatitude_05, decimalLongitude_05)]  # return record with min year
df_maxYr <- setDT(df_NZInPoly)[ , .SD[which.max(year)], by = list(decimalLatitude_05, decimalLongitude_05)]  # return record with max year
df_minMaxYr <- unique(rbind(df_minYr, df_maxYr))  # remove repeated records (cases where min = max year)

# Create a table of just the unique coordinates for performing distance based clustering
df_minMaxYrUnique <- df_minMaxYr %>%
  dplyr::select(species, decimalLongitude_05, decimalLatitude_05) %>%
  distinct()
# Convert decimal degree coordinates in WGS1984 to Mercator as coordinates are in 
# metres
coords <- dismo::Mercator(as.matrix(df_minMaxYrUnique[,c("decimalLongitude_05", "decimalLatitude_05")]))
# Calculate distance matrix
distmat <- dist(data.frame(rownames=rownames(df_minMaxYrUnique), 
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
df_minMaxYrUnique <- data.table(df_minMaxYrUnique, 
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


df_NZInPoly <- merge(x = sf_NZInPoly, 
                     y = df_minMaxYrUnique, 
                     by = c("species", "decimalLatitude_05", "decimalLongitude_05"))

# 9.a.ii. Output native filtered points with defined localities -----#####
fwrite(df_NZInPoly, file = "data/starling/filtered_occurrences/Starling_NZ_1971_2020_withclust.csv", quote = F)

