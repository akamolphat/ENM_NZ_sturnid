# README
This is the README file for all files and folders within `scripts/myna/`

## Overall structure and running order
The scripts in this folder are arranged and run as follows:
1. `01*` to `06*` - These scripts were used to create input files for the analyses (both ENM and NOA)
2. `blockCV_ENM\` folder - Scripts in this folder were run to perform ENM on the common myna. It also creates blocking structure for the spatial cross-validation approach.
3. `07*` to `12*` - These scripts were used to perform NOA on the common myna. 

## File and folder details
This folder consists of the following files:
### Data preparation
* `01*.R` - prepare shapefiles for the mynas native range and New Zealand. The shapefiles are used for plotting, calibration areas, and extracting environmental variables. 
* `02*` - clip environmental variables to shapefiles to create input environmental variable files. This clean up the environmental tiff files. `02_`, `02b_`, `02c` were ran with a slurm script. `02d` was run interactively.
* `03*` - clean myna occurrences.
* `04*` - thin myna occurrences.
* `05*` - converts tiff files to dataframe. This is used for NOA, but also for ease in performing correlations to assess variable collinearity when using the environmental variables for the ENM.
* `06*` - performs correlation analyses (Pearson's; 06b_*.sl include for Spearmans, but not used in the end. Spearman's require a lot more computation power).
### ENM
* `blockCV_ENM\` - contains scripts used to perform ENMs on myna data
### NOA
* 
* 

