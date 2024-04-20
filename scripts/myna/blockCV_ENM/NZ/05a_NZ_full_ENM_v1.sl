#!/bin/bash -e
#SBATCH --job-name=NZ_full_ENMv1
#SBATCH --output=05a_KA_myna_NZ_full_ENMv1_%j.out
#SBATCH --error=05a_KA_myna_NZ_full_ENMv1_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/NZ/05_NZ_full_ENM.R NZ NZ_fullBIOMODv1 v1 4 T,F,T,T,F
