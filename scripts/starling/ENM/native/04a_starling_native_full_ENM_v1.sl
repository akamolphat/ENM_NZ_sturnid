#!/bin/bash -e
#SBATCH --job-name=native_full_ENMv1
#SBATCH --output=04a_KA_%j.out
#SBATCH --error=04a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=12:00:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/ENM/04_starling_full_ENM.R native native_fullBIOMODv1 gdd5,bio12,gsl,tree_cover_pct,pop_dens 0.5 T,T,T,F,F
