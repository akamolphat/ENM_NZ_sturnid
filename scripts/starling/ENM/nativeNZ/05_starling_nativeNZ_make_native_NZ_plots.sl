#!/bin/bash -e
#SBATCH --job-name=nativeNZ_NZ_native_maps
#SBATCH --output=05_KA_%j.out
#SBATCH --error=05_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=01:00:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/ENM/nativeNZ/05_starling_nativeNZ_make_native_NZ_plots.R
