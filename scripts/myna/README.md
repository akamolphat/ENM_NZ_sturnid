# README
This is the README file for all files and folders within `scripts/myna/`

## Overall structure and running order
The scripts in this folder are arranged and run as follows:
1. `01*` to `05*` - These scripts were used to create input files for the analyses (both ENM and NOA)
2. `blockCV_ENM\` folder - Scripts in this folder were run to perform ENM on the common myna. It also creates blocking structure for the spatial cross-validation approach.
3. `06*` to `12*` - These scripts were used to perform NOA on the common myna. 

## File and folder details
This folder consists of the following files:
* `01*.R` - prepare shapefiles for the mynas native range and New Zealand. The shapefiles are used for plotting, calibration areas, and extracting environmental variables. 
* `02*` - clip environmental variables to shapefiles to create input environmental variable files. This clean up the environmental tiff files. `02_`, `02b_`, `02c` were ran with a slurm script. `02d` was run interactively.
* 

