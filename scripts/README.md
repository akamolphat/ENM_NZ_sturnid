# README

This is the README file for folders and files within the **scripts** folder.

This folder contains the following scripts:
* `humboldt70.R` - This is the modified script of the humboldt R package version 1.0.0.420121, downloaded from <https://github.com/jasonleebrown/humboldt/tree/master/R>. Sadly, I did not track all the changes systematically after I downloaded the script. I did not make a lot of changes but git was unable to track the changes systematically, so I have just replaced the original file with the latest version.
  * For the script to work on your cluster, you will have to replace:
    *  `source("/nesi/nobackup/uoa02613/A_Chapter5/scripts/humboldt70.R")`
   with the paths to humboldt70.R on your own cluster.
    *  e.g. `source("my_own_path/humboldt70.R")`
* `make_pop_dens.R` - This script calculates population density from population counts per grid cell.
* `make_pop_dens.sl` - This script runs make_pop_dens.R. Note that the paths in your local environment will change but this is uploaded here for completeness. 
