#!/bin/bash -e
#SBATCH --job-name=combine_cal_area
#SBATCH --output=02c_KA_myna_%j.out
#SBATCH --error=02c_KA_myna_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=1:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/02c_myna_comb_area.R
