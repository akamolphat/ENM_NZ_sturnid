#!/bin/bash -e
#SBATCH --job-name=extractenv
#SBATCH --output=KA_envRast2df_%j.out
#SBATCH --error=KA_envRast2df_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=0:10:00
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/05_envRast2df.R
