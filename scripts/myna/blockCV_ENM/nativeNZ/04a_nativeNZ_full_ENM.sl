#!/bin/bash -e
#SBATCH --job-name=nativeNZ_full_ENMv1a
#SBATCH --output=04a_KA_%j.out
#SBATCH --error=04a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=16:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/nativeNZ/04_nativeNZ_full_ENM.R nativeNZ nativeNZ_fullBIOMODv1a gdd5,bio12,gsl,tree_cover_pct,pop_dens 8 T,F,T,T,F
