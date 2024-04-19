# README
This is the README file for all files and folders within `scripts/starling/`

## Overall structure and running order
The scripts in this folder are arranged and run as follows:
1. `01*` to `06*` - These scripts were used to create input files for the analyses (both ENM and NOA)
2. `ENM\` folder - Scripts in this folder were run to perform ENM on the starling. It also creates blocking structure for the spatial cross-validation approach.
3. `09*` - These scripts were used to perform NOA on the starling. 

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

There were numerous other scripts similar to `09_starling_humboldt_UKNZ.R` which were performed to assess the amount of resources (number of cores, run time, and possibility of running based on the whole native range) required for these runs but were not uploaded as it makes things more messy. Hence the `07` and `08` prefixed scripts were not uploaded. 

* `09*` - `09v_starling_humboldt_UKNZ_v3_200reps.sl` calls `09_starling_humboldt_UKNZ.R` to perform NOA based on the gsl, bio12, and gdd5 with 200 replicates for the background and significance test. It uses the outputs from `05b_*` script as inputs.

