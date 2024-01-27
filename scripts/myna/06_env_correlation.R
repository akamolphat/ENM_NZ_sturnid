# explore_climate_var_correlation.R
# 
# Explore climatic variables
#
# This script explore the climate variables that are important to 
# the common myna
#
# 1. Read in data
# 2. Perform correlation analysis. 
#     Output result in correlogram plot and correlation matrix.
#    a. Native + invasive occurrences
#    b. Native occurrences only
#    c. Native calibration points
#    d. Invasive occurrences only
#    e. Invasive calibration points
#    f. All available points
#
# 3. Output to PDF and xlsx file
#
args <- commandArgs(trailingOnly = TRUE)
cormethod <- args[1]
vargrp <- args[2]
if (!(vargrp %in% c("humanVar", "all", "climateVar", "subset1", "subset2", "subset3"))){
  print("argument 2 incorrect, must be humanVar, all, climateVar, subset1, subset2, or subset3")
  quit()
}

# Load libraries ----------------------------------------------------#####
library(data.table)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(xlsx)

# Read in data ------------------------------------------------------#####
df <- fread(file = "data/myna/env_var_df/NativeNZ_1km_var_with_thinned_occ.csv")

# 2. Correlation ----------------------------------------------------#####
print(paste("Start ", cormethod, "'s correlation", sep = ""))
## 2.a. Native + NZ occurrences -------------------------------------#####
print("Native + NZ occurrences")
if (vargrp == "all"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(bio1:log_pop_dens) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
} else if (vargrp == "humanVar"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(urb_frac_2000:log_pop_dens) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
} else if (vargrp == "climateVar"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(bio1:gdd5) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
} else if (vargrp == "subset1"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(c(bio1, bio5, bio6, bio12, bio18, gsl, gsp, gst, gdd5, bio10, pop_dens_2000, log_pop_dens, tree_cover_pct)) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
} else if (vargrp == "subset2"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(c(bio1, bio12, gsl, gsp, bio10, log_pop_dens, tree_cover_pct)) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
} else if (vargrp == "subset3"){
  mat_cor_a <- df %>%
    dplyr::filter(occurrences == 1) %>%
    dplyr::select(c(bio12, gsl, gdd5, log_pop_dens, tree_cover_pct)) %>%
    as.matrix() %>%
    rcorr(type = cormethod)
}


pa <- corrplot(mat_cor_a$r, type = "upper", order = "hclust", addCoef.col = "black",
               tl.col = "black", tl.srt = 45, title = "Native and NZ occurrences", mar=c(0,0,1,0))

colorder <- colnames(pa$corr)
## 2.b. Native occurrences ------------------------------------------#####
print("Native occurrences")
mat_cor_b <- df %>%
  dplyr::filter(range == "native") %>%
  dplyr::filter(occurrences == 1) %>%
  dplyr::select(all_of(colorder)) %>%
  as.matrix() %>%
  rcorr(type = cormethod)

## 2.c. Native calibration ------------------------------------------#####
print("Native calibration")
mat_cor_c <- df %>%
  dplyr::filter(range == "native") %>%
  dplyr::select(all_of(colorder)) %>%  # reorder by order as in p
  as.matrix() %>%
  rcorr(type = cormethod)

## 2.d. Invasive occurrences ----------------------------------------#####
print("NZ occurrences")
mat_cor_d <- df %>%
  dplyr::filter(range == "NZ") %>%
  dplyr::filter(occurrences == 1) %>%
  dplyr::select(all_of(colorder)) %>%
  as.matrix() %>%
  rcorr(type = cormethod)

## 2.e. Invasive calibration ----------------------------------------#####
print("NZ calibration")
mat_cor_e <- df %>%
  dplyr::filter(range == "NZ") %>%
  dplyr::select(all_of(colorder)) %>%
  as.matrix() %>%
  rcorr(type = cormethod)

## 2.f. Native + Invasive calibration -------------------------------#####
print("Native + NZ calibration")
mat_cor_f <- df %>%
  dplyr::select(all_of(colorder)) %>%
  as.matrix() %>%
  rcorr(type = cormethod)

# 3. Output correlation coef. ---------------------------------------#####
print(paste("Output ", cormethod, "'s correlation plot", sep = ""))
pdf(paste("results/myna/env_correlation/", vargrp, "_", cormethod, "_coef.pdf", sep = ""))
pa <- corrplot(mat_cor_a$r, type = "upper", order = "hclust", addCoef.col = "black", number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "Native and NZ occurrences", mar=c(0,0,1,0))
pb <- corrplot(mat_cor_b$r, type = "upper", order = "original", addCoef.col = "black",number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "Native occurrences", mar=c(0,0,1,0))
pc <- corrplot(mat_cor_c$r, type = "upper", order = "original", addCoef.col = "black", number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "Native calibration area", mar=c(0,0,1,0))
pd <- corrplot(mat_cor_d$r, type = "upper", order = "original", addCoef.col = "black", number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "NZ occurrences", mar=c(0,0,1,0))
pe <- corrplot(mat_cor_e$r, type = "upper", order = "original", addCoef.col = "black", number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "NZ calibration", mar=c(0,0,1,0))
pf <- corrplot(mat_cor_f$r, type = "upper", order = "original", addCoef.col = "black", number.cex = 0.3,
               tl.col = "black", tl.srt = 45, title = "Native and NZ calibration area", mar=c(0,0,1,0))
dev.off()

print(paste("Output ", cormethod, "'s correlation to excel", sep = ""))
xlsx_file <- paste("results/myna/env_correlation/", vargrp, "_", cormethod, "_coef.xlsx", sep = "")

write.xlsx(x = data.frame(pa$corr), file = xlsx_file, row.names = T, sheetName = "R_Native_NZ_occ")
write.xlsx(x = data.frame(mat_cor_a$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_Native_NZ_occ", append = T)

write.xlsx(x = data.frame(pb$corr), file = xlsx_file, row.names = T, sheetName = "R_Native_occ", append = T)
write.xlsx(x = data.frame(mat_cor_b$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_Native_occ", append = T)

write.xlsx(x = data.frame(pc$corr), file = xlsx_file, row.names = T, sheetName = "R_Native_cal", append = T)
write.xlsx(x = data.frame(mat_cor_c$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_Native_cal", append = T)

write.xlsx(x = data.frame(pd$corr), file = xlsx_file, row.names = T, sheetName = "R_NZ_occ", append = T)
write.xlsx(x = data.frame(mat_cor_d$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_NZ_occ", append = T)

write.xlsx(x = data.frame(pe$corr), file = xlsx_file, row.names = T, sheetName = "R_NZ_cal", append = T)
write.xlsx(x = data.frame(mat_cor_e$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_NZ_cal", append = T)

write.xlsx(x = data.frame(pf$corr), file = xlsx_file, row.names = T, sheetName = "R_Native_NZ_cal", append = T)
write.xlsx(x = data.frame(mat_cor_f$P[colorder, colorder]), file = xlsx_file, row.names = T, sheetName = "pval_Native_NZ_cal", append = T)

