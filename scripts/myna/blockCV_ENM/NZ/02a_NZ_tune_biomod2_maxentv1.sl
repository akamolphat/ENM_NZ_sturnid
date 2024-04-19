#!/bin/bash -e
#SBATCH --job-name=NZ_tune_biomod2_maxentv1
#SBATCH --output=02a_KA_myna_NZ_tune_biomod2_maxentv1_%j.out
#SBATCH --error=02a_KA_myna_NZ_tune_biomod2_maxentv1_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/NZ/02_NZ_tune_biomod2_maxent.R NZ v1
