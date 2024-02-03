#!/bin/bash -e
#SBATCH --job-name=clip_env_NZ
#SBATCH --output=02b_KA_%j.out
#SBATCH --error=02b_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:15:00
#SBATCH --mem=4G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/02b_starling_clip_env_NZ_cal_area.R
