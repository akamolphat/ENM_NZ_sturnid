#!/bin/bash -e
#SBATCH --job-name=make_future_NZ_ssp126_ssp1
#SBATCH --output=06a_%j.out
#SBATCH --error=06a_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=1:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/make_future_projections.R myna nativeNZ_fullBIOMODv1a NZ ssp126 ssp1
