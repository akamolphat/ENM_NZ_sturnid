#!/bin/bash -e
#SBATCH --job-name=clip_env_cal
#SBATCH --output=KA_myna_clip_env_cal_%j.out
#SBATCH --error=KA_myna_clip_env_cal_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:40:00
#SBATCH --mem=8G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/02_myna_clip_env_calibration_area.R
