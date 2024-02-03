# README
This is the README file for all files and folders within `scripts/starling/`

## Overall structure and running order
The scripts in this folder are arranged and run as follows:
1. `01*` to `06*` - These scripts were used to create input files for the analyses (both ENM and NOA)
2. `ENM\` folder - Scripts in this folder were run to perform ENM on the starling. It also creates blocking structure for the spatial cross-validation approach.
3. `07*` to `09*` - These scripts were used to perform NOA on the starling. 

## File and folder details
This folder consists of the following files:
### Data preparation
* `01*.R` - prepare shapefiles for the starling native range and New Zealand. The shapefiles are used for plotting, calibration areas, and extracting environmental variables. 
* `02*` - clip environmental variables to shapefiles to create input environmental variable files. This clean up the environmental tiff files. `02_`, `02b_`, `02c` were ran with a slurm script. `02d` was run interactively.
* `03*` - clean starling occurrences.
* `04*` - thin starling occurrences.
* `05*` 
  * `05_*` - converts tiff files to dataframe, but only for subsets of the dataset. This is because the native range is too large. This is for performing correlations to assess variable collinearity when using the environmental variables for the ENM.
  * `05b_*` - converts tiff files to dataframe for UK and NZ. This is used for NOA.
* `06*` - performs correlation analyses.
### ENM
* `ENM\` - contains scripts used to perform ENMs on myna data
### NOA
*  
* 

