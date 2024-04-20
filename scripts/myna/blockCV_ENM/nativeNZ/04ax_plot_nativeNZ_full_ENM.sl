#!/bin/bash -e
#SBATCH --job-name=nativeNZ_full_ENMv1a
#SBATCH --output=04ax_KA_%j.out
#SBATCH --error=04ax_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=6:00:00
#SBATCH --mem=128G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/plot_model_projections.R nativeNZ nativeNZ_fullBIOMODv1a 
