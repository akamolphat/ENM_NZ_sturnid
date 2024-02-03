#!/bin/bash -e
#SBATCH --job-name=env_cor
#SBATCH --output=06a_KA_%j.out
#SBATCH --error=06a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:20:00
#SBATCH --mem=8G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/06_starling_env_sub_correlation.R pearson all
# Rscript scripts/starling/06_starling_env_sub_correlation.R pearson humanVar
# Rscript scripts/starling/06_starling_env_sub_correlation.R pearson climateVar
Rscript scripts/starling/06_starling_env_sub_correlation.R pearson subset1
Rscript scripts/starling/06_starling_env_sub_correlation.R pearson subset2
# Rscript scripts/starling/06_starling_env_sub_correlation.R pearson subset3

