#!/bin/bash -e
#SBATCH --job-name=NZv1_evalnat
#SBATCH --output=05a_%j.out
#SBATCH --error=05a_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=1-00:00:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613
#SBATCH --profile task

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/ENM/05_starling_model_eval_proj.R NZ NZ_fullBIOMODv1 native v1

