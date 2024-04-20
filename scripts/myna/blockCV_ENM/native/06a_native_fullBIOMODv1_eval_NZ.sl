#!/bin/bash -e
#SBATCH --job-name=Natv1_evalNZ
#SBATCH --output=06a_KA_myna_natv1_evalNZ_%j.out
#SBATCH --error=06a_KA_myna_natv1_evalNZ_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=2:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613
#SBATCH --profile task

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/myna/blockCV_ENM/model_eval_proj.R native native_fullBIOMODv1 NZ v1
