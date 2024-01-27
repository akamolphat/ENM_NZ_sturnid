#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 4) {
  stop("Four argument must be supplied as follows: \n
       range (e.g. native) \n
       runName (e.g. native_BIOMODv1) \n
       evalrange (e.g. NZ)", call.=FALSE)
} else {
  range <- args[1]  # "native"
  runName <- args[2]  # "native_BIOMODv4"
  evalrange <- args[3]  # "NZ"
}
# model_eval_proj ---------------------------------------------------#####
# Load libraries ----------------------------------------------------#####
library(terra)
library(raster)
library(sf)
library(biomod2)
library(tidyverse)
library(data.table)
# Define run name and models ----------------------------------------#####
# range <- "NZ"
# runName <- "NZ_fullBIOMODv1"
# evalrange <- "native"
# version <- "v1"
dir.create(paste("data/starling/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/starling/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)

# Define .Rdata files -----------------------------------------------#####
biomodOutRdata <- paste("data/starling/BIOMOD/", range, "/", runName, "/myBiomodModelOut_", runName, ".Rdata", sep = "")
# varResponseOutput <- paste("results/starling/BIOMOD/", range, "/", runName, "/response_plots_", runName, ".pdf", sep = "")
outevalCSV <- paste("results/starling/BIOMOD/", range, "/", runName, "/", runName, "_Eval_", evalrange, ".csv", sep = "")
print(outevalCSV)
# Load biomod model output ------------------------------------------#####
load(file = biomodOutRdata)
ls_varnames <- myBiomodModelOut@expl.var.names

# Get built models and see if they are present
ls_builtModels <- get_built_models(myBiomodModelOut)

# Evaluate with evalrange data --------------------------------------#####
biomodDataRdata <- paste("data/starling/BIOMOD/", evalrange, "/", evalrange, "_myBiomodData.Rdata", sep = "")
load(biomodDataRdata)

# Subset env var ----------------------------------------------------
if (all(ls_varnames %in% colnames(myBiomodData@data.env.var))){
  print("All input variables are found in input BIOMOD data")
} else {
  stop(paste("The following variables are not found in input BIOMOD data:", paste(ls_varnames[!ls_varnames %in% colnames(myBiomodData@data.env.var)], sep = ", ")))
}
myBiomodData@data.env.var <- myBiomodData@data.env.var[ls_varnames]

dt_eval <- cbind(starling = myBiomodData@data.species[myBiomodData@PA$PA1],
                 myBiomodData@data.env.var[myBiomodData@PA$PA1,])
dt_eval$starling[is.na(dt_eval$starling)] <- 0  # Assign zeros instead of NA

# Cross evaluate ----------------------------------------------------
eval_test <- biomod2::evaluate(myBiomodModelOut, data=dt_eval, stat=c('ROC','TSS','KAPPA', 'BIAS')) # This code does not seem to work in interactive mode on NeSI
print(eval_test)

# Output to csv file ------------------------------------------------#####
ctr <- 1
for (nm in names(eval_test)){
  print(nm)
  if (ctr == 1){
    dt_eval_res <- data.frame(eval_test[[nm]])
    dt_eval_res$mod <- nm
    dt_eval_res$eval_stat <- rownames(dt_eval_res)
  } else {
    dt_eval_res2 <- data.frame(eval_test[[nm]])
    dt_eval_res2$mod <- nm
    dt_eval_res2$eval_stat <- rownames(dt_eval_res2)
    dt_eval_res <- rbind(dt_eval_res, dt_eval_res2)
  }
  ctr <- ctr + 1
}
dt_eval_res
write_csv(x = dt_eval_res, file = outevalCSV)

# Make cross projection maps ----------------------------------------
ls_mod <- c('GLM',
            'GAM',
            'SRE', 
            'MAXENT.Phillips',
            'MAXENT.Phillips.2')
PDFprojOutput <- paste("results/starling/BIOMOD/", range, "/", runName, "/", runName, "_proj_", evalrange, ".pdf", sep = "")

# Read in raster files
ls_var_file <- ls_varnames
ls_var_file[ls_var_file == "pop_dens"] <- "pop_dens_2000"
varpath <- c(paste("data/starling/", evalrange, "_calibration_area/", ls_var_file, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
## Remove any NA cell -----------------------------------------------
r_var <- mask(r_var, calc(r_var,fun = sum))  # Create a mask to remove any cells with NA
r_var <- stack(r_var)
# Assign variable names
names(r_var) <- ls_varnames
print(names(r_var))
# Output to PDF -----------------------------------------------------
pdf(PDFprojOutput, width = 11.7, height = 8.3)
for (mod in ls_mod){
  ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
  if (length(ls_modPlot) > 0){
    projname <- paste(runName, "_proj", evalrange, "_", mod, sep = "")
    myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                            new.env = r_var,
                                            proj.name = projname,
                                            selected.models = ls_modPlot,
                                            # binary.meth = c('ROC'),
                                            compress = FALSE,
                                            build.clamping.mask = T)
    plot(myBiomodProjection, str.grep = mod)
  } else {
    print(paste(mod, " model not built.", sep = ""))
  }
  
}
dev.off()
