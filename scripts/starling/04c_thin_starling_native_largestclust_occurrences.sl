#!/bin/bash -e
#SBATCH --job-name=thinnativelargest
#SBATCH --output=04c_KA_%j.out
#SBATCH --error=04c_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=2:00:00
#SBATCH --mem=250G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/04c_thin_starling_native_largestclust_occurrences.R
