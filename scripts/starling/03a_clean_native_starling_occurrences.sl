#!/bin/bash -e
#SBATCH --job-name=clean_starling_occ
#SBATCH --output=03a_KA_%j.out
#SBATCH --error=03a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:30:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/03a_clean_native_starling_occurrences.R
