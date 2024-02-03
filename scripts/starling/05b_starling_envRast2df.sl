#!/bin/bash -e
#SBATCH --job-name=star_extractenv
#SBATCH --output=05b_KA_%j.out
#SBATCH --error=05b_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=15:00:00
#SBATCH --mem=200G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/05b_starling_envRast2df.R

