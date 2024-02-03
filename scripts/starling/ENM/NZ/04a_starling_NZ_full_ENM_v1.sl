#!/bin/bash -e
#SBATCH --job-name=NZ_full_ENMv1
#SBATCH --output=04a_KA_%j.out
#SBATCH --error=04a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=4:00:00
#SBATCH --mem=8G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/ENM/04_starling_full_ENM.R NZ NZ_fullBIOMODv1 gdd5,bio12,gsl,tree_cover_pct,pop_dens 0.5 T,F,F,F,F
