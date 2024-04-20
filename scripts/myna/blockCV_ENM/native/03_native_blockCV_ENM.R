#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 5) {
  stop("Six argument must be supplied as follows: \n
       range (e.g. native) \n
       runName (e.g. native_BIOMODv1) \n
       inputversion (e.g. v1) \n
       betamult (e.g. 0.5) \n
       lqpht (e.g. T,T,T,F,T)", call.=FALSE)
} else {
  range <- args[1]  # "native"
  runName <- args[2]  # "native_BIOMODv4"
  inputversion <- args[3]  # "v4"
  # ls_varnames <- unlist(strsplit(args[4], split = ","))
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
dir.create(paste("data/myna/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)

# Define .Rdata files -----------------------------------------------#####
sbRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_sb_blocking.Rdata", sep = "")
biomodDataRdata <- paste("data/myna/BIOMOD/", range, "/", range, "_myBiomodData", inputversion, ".Rdata", sep = "")
biomodOutRdata <- paste("data/myna/BIOMOD/", range, "/", runName, "/myBiomodModelOut_", runName, ".Rdata", sep = "")
modelScoresOutput <- paste("results/myna/BIOMOD/", range, "/", runName, "/model_scores_", runName, ".pdf", sep = "")
outvarimpCSV <- paste("results/myna/BIOMOD/", range, "/", runName, "/var_imp_", runName, ".csv", sep = "")
varResponseOutput <- paste("results/myna/BIOMOD/", range, "/", runName, "/response_plots_", runName, ".pdf", sep = "")
PDFprojOutput <- paste("results/myna/BIOMOD/", range, "/", runName, "/model_projections_", runName, ".pdf", sep = "")

# Load .Rdata input -------------------------------------------------#####
print("Loading data")
load(file = biomodDataRdata)
load(file = sbRdata)

# Create list of variables from input data --------------------------#####
ls_varnames <- colnames(myBiomodData@data.env.var)

# 2. Defining the folds for DataSplitTable --------------------------#####
# note that biomodTable should be used here not folds
# use generated folds from spatialBlock in previous section
DataSplitTable <- sb$biomodTable
# 3. Defining Models Options with help from Valavi et al. 2022 ------#####
#
## 3.a. Extract occurrences from myBiomodData -----------------------#####
# Define function to get data from myBiomodData
get_PAtab <- function(bfd){
  # Function from: http://rstudio-pubs-static.s3.amazonaws.com/416446_3ef37751ae1e4e569964dabc09a75b56.html
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA # Only if there are PA.nb.rep
  )
}

# Extract number of presences and background points -----------------#####
# sf_spAll <- get_PAtab(myBiomodData) %>%
#   dplyr::select(status, x, y) %>%
#   rename(species = status) %>%
#   st_as_sf(coords = c("x", "y"), crs = crs(sf_cal)) # convert to sf
# Options from Valavi et al. 2022, Codes in appendix. Transfer to biomod2
# GAM, and MAXENT options are somewhat transferred directly
# GLM is not direct as Valavi et al. 2022 used a stepGAM to select
# calculating the case weights (down-weighted or equal weights)
prNum <- get_PAtab(myBiomodData) %>%
  dplyr::filter(status == 1) %>%
  nrow()

# get_PAtab(myBiomodData) %>%
#   dplyr::filter(is.na(status)) %>%
#   dplyr::filter(PA2 == T) %>%
#   nrow()
# wt <- ifelse(model_data$occ == 1, 1, prNum / bgNum)
# Formula for GAM
myGAMform <- formula(paste("myna ~", paste(paste0("s(", ls_varnames, ")"), collapse = " + ")))
# Subsampling for RF
samsize <- c("0" = prNum, "1" = prNum)

# Parsing maxentlqpht to actual arguments
maxentlqpht <- as.logical(maxentlqpht)

# Customise BIOMOD_Modelling options.
myBiomodOption <- BIOMOD_ModelingOptions(
  GLM = list(type = 'quadratic',  # as default, only want unimodal relationships anyways
             interaction.level = 0,  # 0 as default
             # weights = wt,
             family = binomial(link = "logit")),
  GAM = list(algo = "GAM_mgcv",
             myFormula = myGAMform,
             family = binomial(link = "logit"),
             # weights = wt,
             method = "REML"),
  MAXENT.Phillips = list(path_to_maxent.jar = path2maxent,
                         memory_allocated = NULL,
                         betamultiplier = betamult,
                         linear = maxentlqpht[1],
                         quadratic = maxentlqpht[2],
                         product = maxentlqpht[3],
                         hinge = maxentlqpht[4],
                         threshold = maxentlqpht[5])
  # Cannot set options for maxnet (MAXENT.Phillips.2)
)

# myBiomodOption <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(path_to_maxent.jar = path2maxent))

# 4. Model fitting --------------------------------------------------#####
# Model weights on background vs presence points are done here.
# However, the default seems to be what I want anyways, 
# argument "Prevalence = 0.5".
print("Start BIOMOD_Modeling.")
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                     models = ls_mod,
                                     # models = c("SRE"),
                                     models.options = myBiomodOption,
                                     DataSplitTable = sb$biomodTable, # blocking folds
                                     VarImport = 10, # Change this to >1 to get variable importance estimates
                                     models.eval.meth = c('ROC', 'TSS', 'KAPPA'),
                                     do.full.models = F,
                                     modeling.id = runName)

# Save output to .Rdata
save(myBiomodModelOut, file = biomodOutRdata)
# load(file = biomodOutRdata)
# 5. Model evaluation -----------------------------------------------#####
# get all models evaluation
print("Getting model evaluations.")
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
apply(myBiomodModelEval["ROC","Testing.data",,,], c(1,2), mean)
mod_score_ls <- c("ROC", "TSS", "KAPPA")

# Make some plots of model scores
pdf(modelScoresOutput)
for (i in seq(1, (length(mod_score_ls) - 1))){
  for (j in seq(i + 1, length(mod_score_ls))){
    models_scores_graph(
      myBiomodModelOut,
      by = "models",
      metrics = c(mod_score_ls[i], mod_score_ls[j]),
      xlim = c(0,1),
      ylim = c(0,1)
    )
  }
}

dev.off()

# Get an idea of the variable importance
df_varImp <- get_variables_importance(obj = myBiomodModelOut)
# Average variable
varimp <- apply(df_varImp, c(1,2), mean)
varimp <- data.frame(varimp)
varimp$var <- myBiomodModelOut@expl.var.names
varimp <- varimp %>%
  relocate(var, .before = GLM)
write_csv(varimp, file = outvarimpCSV)

# Load different models and plot response plots ---------------------#####
ls_builtModels <- get_built_models(myBiomodModelOut)
print("Plotting response plots.")
pdf(varResponseOutput)
for (mod in ls_mod){
  ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
  if (length(ls_modPlot) > 0){
    myMOD <- BIOMOD_LoadModels(myBiomodModelOut, models = mod)
    ##' plot 2D response plots
    myMODRespPlot2D <-
      response.plot2(
        models = myMOD,
        Data = get_formal_data(myBiomodModelOut, 'expl.var'),
        show.variables = get_formal_data(myBiomodModelOut,'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = 'median',
        col = rep(c("blue", "red", "green", "black", "purple"), 2),
        lty = c(rep("solid", 5), rep("dotted", 5)),
        legend = TRUE,
        data_species = get_formal_data(myBiomodModelOut, 'resp.var')
      )
  }
}
dev.off()

# Project -----------------------------------------------------------#####
## Read tiff files --------------------------------------------------#####
ls_var_file <- ls_varnames
ls_var_file[ls_var_file == "pop_dens"] <- "pop_dens_2000"
varpath <- c(paste("data/myna/", range, "_calibration_area/", ls_var_file, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
# Assign variable names
names(r_var) <- ls_varnames
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
                                            binary.meth = 'TSS',
                                            compress = FALSE,
                                            build.clamping.mask = FALSE)
    plot(myBiomodProjection, str.grep = mod)
  } else {
    print(paste(mod, " model not built.", sep = ""))
  }
  
}
dev.off()

