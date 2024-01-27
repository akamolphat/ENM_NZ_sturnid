#!/bin/bash -e
#SBATCH --job-name=env_cor_spearman
#SBATCH --output=KA_env_correlation_spearman_%j.out
#SBATCH --error=KA_env_correlation_spearman_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=8:00:00
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/06_env_correlation.R spearman climateVar
