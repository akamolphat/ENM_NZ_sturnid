#!/bin/bash -e
#SBATCH --job-name=native_full_ENMv1
#SBATCH --output=05a_KA_myna_native_full_ENMv1_%j.out
#SBATCH --error=05a_KA_myna_native_full_ENMv1_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=6:00:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/native/05_native_full_ENM.R native native_fullBIOMODv1 v1 8 T,T,T,T,F
