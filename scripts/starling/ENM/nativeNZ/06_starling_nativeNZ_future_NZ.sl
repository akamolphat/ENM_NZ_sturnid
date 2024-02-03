#!/bin/bash -e
#SBATCH --job-name=make_future_NZ
#SBATCH --output=06_%j.out
#SBATCH --error=06_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=ALL
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/make_future_projections.R starling nativeNZ_fullBIOMODv1 NZ ssp126 ssp1
Rscript scripts/make_future_projections.R starling nativeNZ_fullBIOMODv1 NZ ssp370 ssp3
Rscript scripts/make_future_projections.R starling nativeNZ_fullBIOMODv1 NZ ssp370 none
