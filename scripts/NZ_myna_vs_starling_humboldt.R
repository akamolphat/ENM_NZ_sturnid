# 09_starling_humboldt_UKNZ_compare_ncpus_reps.R
# 
# This script performs NOA on the full dataset at different
# cpus-per-thread and e.reps/b.reps to estimate how this would scale up 
# with more reps.
#
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Exactly four argument must be supplied as follows: \n
       version (e.g. v1a, v1b, v1c, etc.)\n
       ls_var (e.g. gdd5,bio12,gsl,pop_dens)\n
       reps (e.g. 10)\n
       comp_area (e.g. full or ae)", call.=FALSE)
} else {
  version <- args[1]  # "v1a"
  ls_var <- unlist(strsplit(args[2], split = ","))
  reps <- as.integer(args[3])
  comp_area <- args[4]
}
if (!(comp_area %in% c("full", "ae"))){
  stop("4th argument (comp_area) must be either be:\n
       'full' or\n
       'ae'.")
}
cores_n <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK'))
print(paste("ncores =", cores_n))
# version <- "v1b"
# ls_var <- c("gdd5", "bio12", "gsl", "pop_dens")
# Load libraries ----------------------------------------------------
library(data.table)
library(tidyverse)
# Source custom humboldt file ---------------------------------------
source("scripts/humboldt70_custom.R")
# Read in environmental variables -----------------------------------
inputcsv1 <- "data/myna/env_var_df/NZ_1km_var_with_thinned_occ.csv"
inputcsv2 <- "data/starling/env_var_df/NZ_1km_var_with_thinned_occ.csv"
# inputcsv <- "data/starling/env_var_df/NZ_1km_var_with_thinned_occ.csv"
df_comb1 <- fread(inputcsv1)
df_comb2 <- fread(inputcsv2)
ls_col <- colnames(df_comb2)
df_comb1 <- df_comb1[,..ls_col]
df_comb1$species <- "myna"
df_comb2$species <- "starling"
df_comb <- rbind(df_comb1, df_comb2)
df_comb <- df_comb %>%
  rename(pop_dens = pop_dens_2000)
# Select only relevant columns --------------------------------------
ls_AllAxisCol <- c("x", "y", ls_var, "occurrences", "species")
df_comb <- df_comb[,..ls_AllAxisCol]
# Get an idea of number of points in the different species ------------
sum(df_comb$species == "starling")
sum(df_comb$species == "myna")
sum(df_comb$species == "myna")/sum(df_comb$species == "starling")

# Extract environmental variables -----------------------------------
ls_axisCol <- c("x", "y", ls_var)

# Extract variables at species occurrences
scores.sp.myna <- data.frame(df_comb[df_comb$species == "myna" & df_comb$occurrences == 1, ..ls_axisCol])  # UK occurrences
scores.sp.star <- data.frame(df_comb[df_comb$species == "starling" & df_comb$occurrences == 1, ..ls_axisCol])  # invasive occurrences

# Calibration area
scores.clim.myna <- data.frame(df_comb[df_comb$species == "myna", ..ls_axisCol])  # UK occurrences
scores.clim.star <- data.frame(df_comb[df_comb$species == "starling", ..ls_axisCol])  # NZ occurrences

# Global area
scores.globclim <- data.frame(df_comb[, ..ls_axisCol])

# Extract just points and species for the humboldt.doitall
# Note that these points must be a subset for scores.clim.star and sores.clim.star

ls_spXY <- c("species", "x", "y")
sp1 <- data.frame(df_comb[df_comb$species == "myna" & df_comb$occurrences == 1, ..ls_spXY]) %>%  # UK distribution
  rename(sp = species)
sp2 <- data.frame(df_comb[df_comb$species == "starling" & df_comb$occurrences == 1, ..ls_spXY]) %>%  # NZ distribution
  rename(sp = species)

rm(df_comb)
gc()

# Perform NOA analyses ----------------------------------------------
# PNTI calculation
# Not needed at this point as this is estimated in doitall and shown on 
# plot *_SPP.pdf
#
# pnt1<- humboldt.pnt.index(scores.globclim[,c(3,4)], scores.clim.myna[,c(3,4)], scores.sp.myna[,c(3,4)], kern.smooth = 1, R = 100)
# pnt2<- humboldt.pnt.index(scores.globclim[,c(3,4)], scores.clim.star[,c(3,4)], scores.sp.star[,c(3,4)], kern.smooth = 1, R = 100)

##run it first with full environmental for background tests and equivalence statistic (total equivalence or divergence in current distributions)
dir.create(paste("results/humboldt_myna_vs_starling/", version, sep = ""), showWarnings = F, recursive = T)
if (comp_area == "full"){
  full <- humboldt.doitall(inname = paste("results/humboldt_myna_vs_starling/", version, "/full_", reps, "reps", sep = ""),
                           env1 = scores.clim.myna,
                           env2 = scores.clim.star,
                           sp1 = sp1,
                           sp2 = sp2,
                           env.reso=0.008333333, reduce.env=0,
                           non.analogous.environments="YES",
                           env.trim=F,
                           correct.env=T,
                           pcx = 1, pcy = 2,
                           col.env = e.var, e.var = c(3:dim(scores.clim.star)[2]),
                           R = 100, kern.smooth = 1,
                           e.reps = reps, b.reps = reps,
                           nae = "YES", thresh.espace.z = 0.0001,
                           p.overlap=F, # Has to be F as plotting requires ks
                           p.boxplot=F, p.scatter=T, run.silent=F, 
                           ncores = cores_n)
  
  save(full, file = paste("results/humboldt_myna_vs_starling/", version, "/full_", reps, "reps_results.Rdata", sep = ""))
  
} else if (comp_area == "ae"){
  ##run it a second time with a trimmed, shared-espace. Here the equivalence statistic tests for niche evolution or niche divergence. For comparing results, change only the following model parameters: reduce.env, non.analogous.environmental, env.trim, nae
  ae <- humboldt.doitall(inname =  paste("results/humboldt_myna_vs_starling/", version, "/ae_", reps, "reps", sep = ""),
                         env1 = scores.clim.myna,
                         env2 = scores.clim.star,
                         sp1=sp1, sp2=sp2,
                         env.reso=0.008333333, reduce.env=2, reductype="STANDARD",
                         non.analogous.environments="NO",
                         env.trim=F,
                         correct.env=T,
                         pcx = 1, pcy = 2,
                         col.env=e.var, e.var=c(3:dim(scores.clim.star)[2]),
                         R=100, kern.smooth=1,
                         e.reps=reps, b.reps=reps,
                         nae="NO", thresh.espace.z=0.0001,
                         p.overlap=F,
                         p.boxplot=F, p.scatter=T,run.silent=F,
                         ncores = cores_n)

  save(ae, file = paste("results/humboldt_myna_vs_starling/", version, "/ae_", reps, "reps_results.Rdata", sep = ""))
  
}
