#!/bin/bash -e
#SBATCH --job-name=thinnative
#SBATCH --output=04a_KA_%j.out
#SBATCH --error=04a_KA_%j.err
#SBATCH --mail-user=kats326@aucklanduni.ac.nz
#SBATCH --mail-type=END
#SBATCH --time=4:00:00
#SBATCH --mem=64G
#SBATCH --ntasks=1
#SBATCH --account=uoa02613

ml R-Geo/4.2.1-gimkl-2022a

cd /nesi/nobackup/uoa02613/A_Chapter5/

Rscript scripts/starling/04_thin_starling_occurrences.R native

# NOTE THAT memory requirement is actuallay about 250GB for one of the cluster
# However, thinning of that particular cluster was done individually as I
# did not want to request 250GB RAM for a long job
