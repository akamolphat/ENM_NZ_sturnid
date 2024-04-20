#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is only one argument: if not, return an error
if (length(args) != 4) {
  stop("Four argument must be supplied as follows: \n
       range (e.g. native) \n
       runName (e.g. native_BIOMODv1) \n
       evalrange (e.g. NZ)\n
       version (e.g. v1)", call.=FALSE)
} else {
  range <- args[1]  # "native"
  runName <- args[2]  # "native_BIOMODv4"
  evalrange <- args[3]  # "NZ"
  version <- args[4]
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
dir.create(paste("data/myna/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)
dir.create(paste("results/myna/BIOMOD/", range, "/", runName, sep = ""), recursive = T, showWarnings = F)

# Define .Rdata files -----------------------------------------------#####
biomodOutRdata <- paste("data/myna/BIOMOD/", range, "/", runName, "/myBiomodModelOut_", runName, ".Rdata", sep = "")
# varResponseOutput <- paste("results/myna/BIOMOD/", range, "/", runName, "/response_plots_", runName, ".pdf", sep = "")
outevalCSV <- paste("results/myna/BIOMOD/", range, "/", runName, "/", runName, "_Eval_", evalrange, ".csv", sep = "")
print(outevalCSV)
# Load biomod model output ------------------------------------------#####
load(file = biomodOutRdata)
ls_varnames <- myBiomodModelOut@expl.var.names

# Get built models and see if they are present
ls_builtModels <- get_built_models(myBiomodModelOut)

# Evaluate with evalrange data --------------------------------------#####
biomodDataRdata <- paste("data/myna/BIOMOD/", evalrange, "/", evalrange, "_myBiomodData",version, ".Rdata", sep = "")
load(biomodDataRdata)

dt_eval <- cbind(myna = myBiomodData@data.species[myBiomodData@PA$PA1],
                 myBiomodData@data.env.var[myBiomodData@PA$PA1,])
dt_eval$myna[is.na(dt_eval$myna)] <- 0  # Assign zeros instead of NA

eval_test <- biomod2::evaluate(myBiomodModelOut, data=dt_eval, stat=c('ROC','TSS','KAPPA', 'BIAS'))
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

# Make projection maps ----------------------------------------------#####
ls_mod <- c('GLM',
            'GAM',
            'SRE', 
            'MAXENT.Phillips',
            'MAXENT.Phillips.2')
PDFprojOutput <- paste("results/myna/BIOMOD/", range, "/", runName, "/", runName, "_proj_", evalrange, ".pdf", sep = "")

# Convert terra rast object to raster for projection
ls_var_file <- ls_varnames
ls_var_file[ls_var_file == "pop_dens"] <- "pop_dens_2000"
varpath <- c(paste("data/myna/", evalrange, "_calibration_area/", ls_var_file, "_cropped_masked.tif", sep = ""))
r_var <- raster::stack(varpath)
## Remove any NA cell -----------------------------------------------
r_var <- mask(r_var, calc(r_var,fun = sum))  # Create a mask to remove any cells with NA
r_var <- stack(r_var)
# Assign variable names
names(r_var) <- ls_varnames
print(names(r_var))

pdf(PDFprojOutput, width = 11.7, height = 8.3)
for (mod in ls_mod){
  ls_modPlot <- ls_builtModels[grep(pattern = paste(mod, "$", sep = ""), ls_builtModels)]
  if (length(ls_modPlot) > 0){
    projname <- paste(runName, "_proj", evalrange, "_", mod, sep = "")
    myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                            new.env = r_var,
                                            proj.name = projname,
                                            selected.models = ls_modPlot,
                                            binary.meth = c('ROC'),
                                            compress = FALSE,
                                            build.clamping.mask = T)
    plot(myBiomodProjection, str.grep = mod)
  } else {
    print(paste(mod, " model not built.", sep = ""))
  }
  
}
dev.off()
