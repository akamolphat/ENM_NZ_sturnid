#!/bin/bash -e
#SBATCH --job-name=nativeNZ_make_data_block
#SBATCH --output=01_KA_%j.out
#SBATCH --error=01_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=2:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/nativeNZ/01_make_nativeNZ_dataset_block.R
