#!/bin/bash -e
#SBATCH --job-name=GBM_NZ_projectNZ
#SBATCH --output=KA_myna_NZ_projectNZGBM_%j.out
#SBATCH --error=KA_myna_NZ_projectNZGBM_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=3:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/test_blockCV_ENM/NZ/04_NZ_projectNZ.R GBM
