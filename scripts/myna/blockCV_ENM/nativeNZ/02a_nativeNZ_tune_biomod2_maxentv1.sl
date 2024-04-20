#!/bin/bash -e
#SBATCH --job-name=nativeNZ_tune_biomod2_maxentv1
#SBATCH --output=02a_KA_%j.out
#SBATCH --error=02a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/nativeNZ/02_nativeNZ_tune_biomod2_maxent.R nativeNZ v1 gdd5,bio12,gsl,tree_cover_pct,pop_dens
