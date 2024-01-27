#!/bin/bash -e
#SBATCH --job-name=nativeNZ_make_data_block
#SBATCH --output=01_KA_%j.out
#SBATCH --error=01_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=1:30:00
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/ENM/nativeNZ/01_nativeNZ_starling_create_dataset_block.R
