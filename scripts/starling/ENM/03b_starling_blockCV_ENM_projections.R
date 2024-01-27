#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 5) {
  stop("Six argument must be supplied as follows: \n
       range (e.g. native) \n
       runName (e.g. native_BIOMODv1) \n
       var_ls (e.g. gdd5,bio12,gsl,pop_dens,tree_cover_pct) \n
       betamult (e.g. 0.5) \n
       lqpht (e.g. T,T,T,F,T)", call.=FALSE)
} else {
  range <- args[1]  # "native"
  runName <- args[2]  # "native_BIOMODv4"
  var_ls <- unlist(strsplit(args[3], split = ","))  # c("gdd5,bio12,pop_dens,tree_cover_pct)
  betamult <- as.numeric(args[4])
  if (is.na(betamult)){
    stop("Maxent betamult is not numeric. This has to be a number with no characters in them.")
  }
  maxentlqpht <- unlist(strsplit(args[5], split = ","))
  if (length(maxentlqpht) != 5){
    stop("Maxent LQPHT argument is not of length = 5")
  } 
  if (length(setdiff(maxentlqpht, c("T", "F"))) != 0){
    stop("Maxent LQPHT argument must be either T or F, and must be supplied as a list separated by commas, e.g. T,F,T,F,T")
  }
}




# Run biomod2 with block CV version 3 (bio5,6) ----------------------#####
# Load libraries ----------------------------------------------------#####
library(blockCV)
library(raster)
library(sf)
library(biomod2)
library(dismo)
library(tidyverse)
library(data.table)
# Define run name and models ----------------------------------------#####
path2maxent <- "/home/kats326/R/gimkl-2022a/4.2/dismo/java/maxent.jar"  # same as dismo
# range <- "native"
# ls_varnames <- c("gdd5", "bio5", "bio6", "bio12", "gsl", 
#                  "log_pop_dens", "tree_cover_pct")
# runName <- "native_BIOMODv4"
ls_mod <- c('GLM',
            'GAM',
            'SRE', 
            'MAXENT.Phillips',
            'MAXENT.Phillips.2')
dir.create(paste("data/starling/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/starling/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)

# Define .Rdata files -----------------------------------------------#####
sbRdata <- paste("data/starling/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
biomodDataRdata <- paste("data/starling/BIOMOD/", range, "/", range, "_myBiomodData.Rdata", sep = "")
biomodOutRdata <- paste("data/starling/BIOMOD/", range, "/", runName, "/myBiomodModelOut_", runName, ".Rdata", sep = "")
modelScoresOutput <- paste("results/starling/BIOMOD/", range, "/", runName, "/model_scores_", runName, ".pdf", sep = "")
outvarimpCSV <- paste("results/starling/BIOMOD/", range, "/", runName, "/var_imp_", runName, ".csv", sep = "")
varResponseOutput <- paste("results/starling/BIOMOD/", range, "/", runName, "/response_plots_", runName, ".pdf", sep = "")
PDFprojOutput <- paste("results/starling/BIOMOD/", range, "/", runName, "/model_projections_", runName, ".pdf", sep = "")

# Load .Rdata input -------------------------------------------------#####
print("Loading data")
load(file = biomodDataRdata)
load(file = sbRdata)

# Subset env var ----------------------------------------------------
if (all(var_ls %in% colnames(myBiomodData@data.env.var))){
  print("All input variables are found in input BIOMOD data")
} else {
  stop(paste("The following variables are not found in input BIOMOD data:", paste(var_ls[!var_ls %in% colnames(myBiomodData@data.env.var)], sep = ", ")))
}
myBiomodData@data.env.var <- myBiomodData@data.env.var[var_ls]

# Create list of variables from input data --------------------------#####
ls_varnames <- colnames(myBiomodData@data.env.var)

load(file = biomodOutRdata)

# Load different models and plot response plots ---------------------#####
ls_builtModels <- get_built_models(myBiomodModelOut)

# Project -----------------------------------------------------------#####
## Read tiff files --------------------------------------------------#####
ls_var_file <- ls_varnames
ls_var_file[ls_var_file == "pop_dens"] <- "pop_dens_2000"
varpath <- c(paste("data/starling/", range, "_calibration_area/", ls_var_file, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
## Remove any NA cell -----------------------------------------------
r_var <- mask(r_var, calc(r_var,fun = sum))  # Create a mask to remove any cells with NA
r_var <- stack(r_var)
# Assign variable names
names(r_var) <- ls_varnames
print(names(r_var))
## Project ----------------------------------------------------------#####
print("Projecting model")
pdf(PDFprojOutput, width = 11.7, height = 8.3)
for (mod in ls_mod){
  ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
  if (length(ls_modPlot) > 0){
    projname <- paste(runName, "_proj_", mod, sep = "")
    myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                            new.env = r_var,
                                            proj.name = projname,
                                            selected.models = ls_modPlot,
                                            # binary.meth = 'TSS',
                                            compress = FALSE,
                                            build.clamping.mask = FALSE)
    plot(myBiomodProjection, str.grep = mod)
  } else {
    print(paste(mod, " model not built.", sep = ""))
  }
  
}
dev.off()

