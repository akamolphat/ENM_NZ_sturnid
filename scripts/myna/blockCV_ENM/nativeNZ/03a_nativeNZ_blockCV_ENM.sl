#!/bin/bash -e
#SBATCH --job-name=nativeNZ_blockCV_ENMv1
#SBATCH --output=03a_KA_%j.out
#SBATCH --error=03a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=1-00:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/nativeNZ/03_nativeNZ_blockCV_ENM.R nativeNZ nativeNZ_BIOMODv1a gdd5,bio12,gsl,tree_cover_pct,pop_dens 8 T,F,T,T,F
