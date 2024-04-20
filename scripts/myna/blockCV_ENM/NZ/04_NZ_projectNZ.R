#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 1) {
  stop("One argument must be supplied (model name, eg. GLM or MAXENT.Phillips).", call.=FALSE)
} else {
  mod <- args[1]
}
# Project BIOMOD2 outputs  ------------------------------------------#####
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
range <- "NZ"
ls_varnames <- c("bio1", "bio5", "bio6", "bio12", "gsl", "bio10", "gsp", 
                 # "urb_frac_2000", 
                 "pop_dens_2000", "ngdc_isa_gcs_resampled")
runName <- "biomodv1b"
# ls_mod <- c('GLM',
#             'GBM', 
#             'GAM',
#             'SRE', 
#             'RF',
#             'MAXENT.Phillips',
#             'MAXENT.Phillips.2')
dir.create(paste("data/test_BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/test_BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)

# Define .Rdata files -----------------------------------------------#####
biomodOutRdata <- paste("data/test_BIOMOD/", range, "/", runName, "/test_myBiomodModelOut_", runName, ".Rdata", sep = "")
modelScoresOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/model_scores_", runName, ".pdf", sep = "")
outvarimpCSV <- paste("results/test_BIOMOD/", range, "/", runName, "/var_imp_", runName, ".csv", sep = "")
varResponseOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/response_plots_", runName, ".pdf", sep = "")

# Load biomod model -------------------------------------------------#####
load(file = biomodOutRdata)
# Get built models and see if they are present
ls_builtModels <- get_built_models(myBiomodModelOut)
ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
if (length(ls_modPlot) == 0){
  stop(paste("Supplied model (", mod,") not in list of built models (from get_built_models).", sep = ""), call.=FALSE)
  
}

# Project -----------------------------------------------------------#####
## Read tiff files --------------------------------------------------#####
varpath <- c(paste("data/myna/", range, "_calibration_area/", ls_varnames, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
# Assign variable names
names(r_var) <- ls_varnames
## Project ----------------------------------------------------------#####

myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = r_var,
                                        proj.name = 'NZ_bin_biomodv1b',
                                        selected.models = ls_modPlot,
                                        # "myna_PA1_RUN5_MAXENT.Phillips.2",
                                        # "myna_PA1_RUN5_GLM",
                                        # "myna_PA1_RUN5_SRE"),
                                        binary.meth = 'TSS',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)
PDFbinaryOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/", mod, "_model_binary_projections_", runName, ".pdf", sep = "")
pdf(PDFbinaryOutput, width = 11.7, height = 8.3)
plot(myBiomodProjection, str.grep = mod)
dev.off()

# myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                         new.env = r_var,
#                                         proj.name = 'NZ_cont_biomodv1b',
#                                         selected.models = ls_modPlot,
#                                         binary.meth = NULL,
#                                         compress = FALSE,
#                                         build.clamping.mask = FALSE)
# PDFcontOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/", mod, "_model_continuous_projections_", runName, ".pdf", sep = "")
# pdf(PDFcontOutput, width = 11.7, height = 8.3)
# plot(myBiomodProjection, str.grep = mod)
# dev.off()

# for (mod in ls_mod){
#   ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
#   if (length(ls_modPlot) > 0){
#     myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                             new.env = r_var,
#                                             proj.name = 'NZ_bin_biomodv1b',
#                                             selected.models = ls_modPlot,
#                                             # "myna_PA1_RUN5_MAXENT.Phillips.2",
#                                             # "myna_PA1_RUN5_GLM",
#                                             # "myna_PA1_RUN5_SRE"),
#                                             binary.meth = 'TSS',
#                                             compress = FALSE,
#                                             build.clamping.mask = FALSE)
#     PDFbinaryOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/", mod, "_model_binary_projections_", runName, ".pdf", sep = "")
#     pdf(PDFbinaryOutput, width = 11.7, height = 8.3)
#     plot(myBiomodProjection, str.grep = mod)
#     dev.off()
#   } else {
#     print(paste(mod, " model not built.", sep = ""))
#   }
#   
# }
# 
# 
# 
# for (mod in ls_mod){
#   # Reproject using binary.meth = NULL
#   ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
#   if (length(ls_modPlot) > 0){
#     myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                             new.env = r_var,
#                                             proj.name = 'NZ_cont_biomodv1b',
#                                             selected.models = ls_modPlot,
#                                             binary.meth = NULL,
#                                             compress = FALSE,
#                                             build.clamping.mask = FALSE)
#     PDFcontOutput <- paste("results/test_BIOMOD/", range, "/", runName, "/", mod, "model_continuous_projections_", runName, ".pdf", sep = "")
#     pdf(PDFcontOutput, width = 11.7, height = 8.3)
#     plot(myBiomodProjection, str.grep = mod)
#     dev.off()
#     
#   } else {
#     print(paste(mod, " model not built.", sep = ""))
#   }
# }
