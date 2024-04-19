#!/bin/bash -e
#SBATCH --job-name=12b_NOA
#SBATCH --output=12b_%j.out
#SBATCH --error=12b_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=3-00:00:00
#SBATCH --mem=200GB
#SBATCH --cpus-per-task=4
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/12_humboldt_myna_fullarea.R v2 gdd5,bio12,gsl 200 full
Rscript scripts/myna/12_humboldt_myna_fullarea.R v2 gdd5,bio12,gsl 200 ae
