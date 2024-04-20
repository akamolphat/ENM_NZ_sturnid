#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 3) {
  stop("Three argument must be supplied as follows: \n
       range (e.g. native) \n
       inputversion (e.g. v1) \n
       var_ls (e.g. gdd5,bio12,gsl)", call.=FALSE)
} else {
  range <- args[1]  # "native"
  inputversion <- args[2]  # "v4"
  var_ls <- unlist(strsplit(args[3], split = ","))
}

# 02_myna_tune_biomod2_maxent.R 
#
# This script tunes maxent as run in biomod2 instead of dismo
# 
# It follows a similar framework as described in Valavi et al. (2022)
# but uses biomod2 instead of dismo, and uses 
# the average of the AUC scores, instead of 
# the stacked AUC scores
#
# The reason for this is stacking or merging the AUC  downgrade 
# classifiers that rank well if they have poor calibration across folds
# See: 
# Forman, G., Scholz, M. & Scholz, M. Apples-to-Apples in Cross-Validation 
# Studies: Pitfalls in Classifier Performance Measurement (2009)
# 
# https://stats.stackexchange.com/questions/386326/appropriate-way-to-get-cross-validated-auc
# 
# This script only tune the model using the PA1 (pseudo-absences dataset 1)
# of the dataset
#
# Load libraries ----------------------------------------------------#####
library(blockCV)
library(raster)
library(sf)
library(biomod2)
library(dismo)
library(tidyverse)
library(data.table)
# Create folder to store folders ------------------------------------#####
dir.create(paste("results/myna/BIOMOD/", range, "/tune_MaxEnt", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("data/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", sep = ""), recursive = T, showWarnings = F)

# Define .Rdata output ----------------------------------------------#####
sbRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
biomodDataRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodData.Rdata", sep = "")

# Define model_id ---------------------------------------------------#####
model_id <- paste(range, "_tune_biomod2_maxent", inputversion, sep = "")

# Define output paths -----------------------------------------------#####
pdfresponseplots <- paste("results/myna/BIOMOD/", range, "/tune_MaxEnt/", range, "_tune_biomod2_maxent", inputversion, ".pdf", sep = "")
outcsv <- paste("results/myna/BIOMOD/", range, "/tune_MaxEnt/", range, "_tune_biomod2_maxent", inputversion, ".csv", sep = "")

# Read shapefile ----------------------------------------------------#####
shpfilepath <- paste("data/myna/native_calibration_area/native_calibration_area.shp", sep = "")
sf_cal <- st_read(dsn = path.expand(shpfilepath))  # Reading shp file for the crs

# Load .Rdata -------------------------------------------------------#####
load(file = biomodDataRdata)
load(file = sbRdata)

# Subset env var ----------------------------------------------------
if (all(var_ls %in% colnames(myBiomodData@data.env.var))){
  print("All input variables are found in input BIOMOD data")
} else {
  stop(paste("The following variables are not found in input BIOMOD data:", paste(var_ls[!var_ls %in% colnames(myBiomodData@data.env.var)], sep = ", ")))
}
myBiomodData@data.env.var <- myBiomodData@data.env.var[,var_ls]

print(colnames(myBiomodData@data.env.var))

# Recreate myBiomodData input with just PA1 -------------------------#####
respvar <- myBiomodData@data.species[myBiomodData@PA$PA1]
explvar <- myBiomodData@data.env.var[myBiomodData@PA$PA1,]
respxy <- myBiomodData@coord[myBiomodData@PA$PA1,]
myBiomodData <- BIOMOD_FormatingData(resp.var = respvar,
                                     expl.var = explvar,
                                     resp.xy = respxy,
                                     resp.name = "myna")

get_respxy <- function(bfd){
  # Function from: http://rstudio-pubs-static.s3.amazonaws.com/416446_3ef37751ae1e4e569964dabc09a75b56.html
  dplyr::bind_cols(
    species = bfd@data.species,
    x = bfd@coord[, 1],
    y = bfd@coord[, 2]
  )
}

sf_spAll <- get_respxy(myBiomodData) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(sf_cal), remove = T) # convert to sf

sb2 <- spatialBlock(speciesData = sf_spAll,
                    species = "species",
                    blocks = sb$blocks,
                    selection = "predefined",
                    foldsCol = "folds",
                    biomod2Format = TRUE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)
# Do this outside of a function instead
blocking_table <- sb2$biomodTable
path2maxent <- "/home/kats326/R/gimkl-2022a/4.2/dismo/java/maxent.jar"
mod <- "MAXENT.Phillips"
# regularisation multipliers
ms <- c(0.5, 1, 2, 3, 4, 5, 6, 7, 8)
# ms <- c(1)
grid <- expand.grid(
  regmult = ms,
  maxentLQPHT = list(
    c(T,F,F,F,F),  # L
    c(F,T,F,F,F),  # Q
    c(T,T,F,F,F),  # LQ
    c(T,F,T,F,F),  # LP
    c(F,T,T,F,F),  # QP
    c(T,T,T,F,F),  # LQP
    c(T,F,F,T,F),  # LH
    c(T,T,F,T,F),  # LQH 
    c(T,F,T,T,F),  # LPH
    c(T,T,T,T,F)),  # LQPH
  stringsAsFactors = FALSE
)
grid[,3] <- rep(c("L","Q","LQ", "LP", "QP", "LQP", "LH", "LQH", "LPH", "LQPH"), each = length(ms))
# grid[,3] <- rep(c("L", "LQP"), each = length(ms))
full_dt <- data.frame()
pdf(pdfresponseplots, width = 11.7, height = 8.3)
for(n in seq_along(grid[,1])){
  print(as.character(unlist(grid[n, ])))
  betamult <- grid[n, 1]
  maxentlqpht <- unlist(grid[n, 2])
  myBiomodOption <- BIOMOD_ModelingOptions(
    MAXENT.Phillips = list(path_to_maxent.jar = path2maxent,
                           memory_allocated = NULL,
                           betamultiplier = betamult,
                           linear = maxentlqpht[1],
                           quadratic = maxentlqpht[2],
                           product = maxentlqpht[3],
                           hinge = maxentlqpht[4],
                           threshold = maxentlqpht[5])
  )
  if(inherits(try(
    myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                         models = mod,
                                         models.options = myBiomodOption,
                                         DataSplitTable = blocking_table, # blocking folds
                                         models.eval.meth = c('ROC'), # Only using ROC
                                         do.full.models = F,
                                         modeling.id = model_id
    )
  ), "try-error")){
    next
  }
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)  # Get ROC estimates
  AUCs <- myBiomodModelEval["ROC","Testing.data",mod,,]
  mean_AUC <- mean(AUCs)
  var_AUC <- var(AUCs)
  sub_dt <- data.frame(cbind(betamult = grid[n, 1], feature_class = grid[n, 3], t(data.frame(AUCs)), mean_AUC, var_AUC))
  full_dt <- rbind(full_dt, sub_dt)
  ## Plot response plots
  print(paste("Betamultiplier = ", grid[n, 1], ", feature class = ", grid[n, 3], sep = ""))
  print(get_built_models(myBiomodModelOut))
  myMOD <- BIOMOD_LoadModels(bm.out=myBiomodModelOut)
  ##' plot 2D response plots
  myMODRespPlot2D <-
    response.plot2(
      models = myMOD,
      Data = get_formal_data(myBiomodModelOut, 'expl.var'),
      show.variables = get_formal_data(myBiomodModelOut,'expl.var.names'),
      do.bivariate = FALSE,
      fixed.var.metric = 'median',
      lty = c("solid", "dotted", "dashed", "dotdash", "longdash"),
      legend = TRUE,
      data_species = get_formal_data(myBiomodModelOut, 'resp.var'),
      main = paste("Betamultiplier = ", grid[n, 1], ", feature class = ", grid[n, 3], sep = "")
    )
  ##
}
dev.off()
best_param <- full_dt[which.max(as.numeric(full_dt$mean_AUC)), 1:2]
print(grid)
print(best_param)
print(full_dt)
full_dt$bestparam <- NA
full_dt$bestparam[which.max(as.numeric(full_dt$mean_AUC))] <- "bestparam"

write_csv(x = full_dt, file = outcsv)

