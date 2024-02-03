#!/bin/bash -e
#SBATCH --job-name=clean_NZ_starling_occ
#SBATCH --output=03b_KA_%j.out
#SBATCH --error=03b_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:10:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/03b_clean_NZ_starling_occurrences.R
