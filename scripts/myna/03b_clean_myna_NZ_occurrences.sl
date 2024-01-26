#!/bin/bash -e
#SBATCH --job-name=clean_NZ_myna
#SBATCH --output=KA_myna_clean_NZ_%j.out
#SBATCH --error=KA_myna_clean_NZ_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:05:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/03b_clean_myna_NZ_occurrences.R
