#!/bin/bash -e
#SBATCH --job-name=clip_env_NZ
#SBATCH --output=KA_myna_clip_env_NZ_%j.out
#SBATCH --error=KA_myna_clip_env_NZ_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:15:00
#SBATCH --mem=4G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/02b_myna_clip_env_NZ_area.R
