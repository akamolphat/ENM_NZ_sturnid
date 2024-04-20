#!/bin/bash -e
#SBATCH --job-name=GBM_native_projectNative
#SBATCH --output=KA_myna_native_projectNativeGBM_%j.out
#SBATCH --error=KA_myna_native_projectNativeGBM_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=24:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/test_blockCV_ENM/native/04_native_projectNative.R GBM
