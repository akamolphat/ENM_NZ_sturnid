#!/bin/bash -e
# Move to folder with thinned by cluster
cd /nesi/nobackup/uoa02613/A_Chapter5/data/myna/filtered_occurrences/native_thinned/16km/
# Combine all CSV to one file
awk '(NR == 1) || (FNR > 1)' *.csv > myna_native_thinned_merged.csv

cd /nesi/nobackup/uoa02613/A_Chapter5/data/myna/filtered_occurrences/NZ_thinned/16km/
# Combined all CSV to one file
awk '(NR == 1) || (FNR > 1)' *.csv > myna_NZ_thinned_merged.csv
