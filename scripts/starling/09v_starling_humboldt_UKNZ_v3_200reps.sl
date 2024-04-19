#!/bin/bash -e
#SBATCH --job-name=09v_NOA
#SBATCH --output=09v_%j.out
#SBATCH --error=09v_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=3:00:00
#SBATCH --mem=10GB
#SBATCH --cpus-per-task=4
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/09_starling_humboldt_UKNZ.R v3 gdd5,bio12,gsl 200 full
Rscript scripts/starling/09_starling_humboldt_UKNZ.R v3 gdd5,bio12,gsl 200 ae
