#!/bin/bash -e
#SBATCH --job-name=env_cor
#SBATCH --output=KA_env_correlation_pearson_%j.out
#SBATCH --error=KA_env_correlation_pearson_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:20:00
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

# Rscript scripts/myna/06_env_correlation.R pearson all
# Rscript scripts/myna/06_env_correlation.R pearson humanVar
# Rscript scripts/myna/06_env_correlation.R pearson climateVar
# Rscript scripts/myna/06_env_correlation.R pearson subset1
# Rscript scripts/myna/06_env_correlation.R pearson subset2
Rscript scripts/myna/06_env_correlation.R pearson subset3
