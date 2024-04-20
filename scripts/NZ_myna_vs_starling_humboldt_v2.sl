#!/bin/bash -e
#SBATCH --job-name=NZ_myna_vs_starv2
#SBATCH --output=myna_vs_star_%j.out
#SBATCH --error=myna_vs_star_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=6:00:00
#SBATCH --mem=16GB
#SBATCH --cpus-per-task=4
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/NZ_myna_vs_starling_humboldt.R v2 gdd5,bio12,gsl 200 full
Rscript scripts/NZ_myna_vs_starling_humboldt.R v2 gdd5,bio12,gsl 200 ae
