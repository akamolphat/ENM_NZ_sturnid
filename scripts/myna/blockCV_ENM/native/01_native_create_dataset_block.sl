#!/bin/bash -e
#SBATCH --job-name=native_make_data_block
#SBATCH --output=01_KA_native_makedatablock_%j.out
#SBATCH --error=01_KA_native_makedatablock_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=1:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/native/01_native_create_dataset_block.R
