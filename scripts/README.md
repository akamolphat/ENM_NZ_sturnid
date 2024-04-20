# README

This is the README file for folders and files within the **scripts** folder.

This folder is arranged as follows:
```
├── myna
│   ├── blockCV_ENM
│   │   └── README.md
│   └── README.md
├── starling
│   ├── ENM
│   │   └── README.md
│   └── README.md
├── humboldt70.R
├── make_pop_dens.R
├── make_pop_dens.sl
├── plot_filtered_thinned_occurrences.R
├── star_myna_varimp_plot.R
├── compare_future_present_proj.R
├── plot_clamping_masks.R
├── NZ_myna_vs_starling_humboldt.R
├── NZ_myna_vs_starling_humboldt_v2.sl
└── README.md
```

This folder contains the following scripts:
* `humboldt70.R` - This is the modified script of the humboldt R package version 1.0.0.420121, downloaded from <https://github.com/jasonleebrown/humboldt/tree/master/R>. Sadly, I did not track all the changes systematically after I downloaded the script. I did not make a lot of changes but git was unable to track the changes systematically, so I have just replaced the original file with the latest version.
  * For the script to work on your cluster, you will have to replace:
    *  `source("/nesi/nobackup/uoa02613/A_Chapter5/scripts/humboldt70.R")`
   with the paths to humboldt70.R on your own cluster.
    *  e.g. `source("my_own_path/humboldt70.R")`
* `make_pop_dens.R` - This script calculates population density from population counts per grid cell.
* `make_pop_dens.sl` - This script runs make_pop_dens.R. Note that the paths in your local environment will change but this is uploaded here for completeness.
* `plot_filtered_thinned_occurrences.R` - This script makes **Figure 1** of the main text.
* `star_myna_varimp_plot.R` - This script makes **Figure 2** of the main text.
* `compare_future_present_proj.R` - This script makes **Figures 3 and 4** of the main text.
* `plot_clamping_masks.R` - This script makes **Figure S2.5** in the Supplementary material.
* `NZ_myna_vs_starling_humboldt_v2.sl` calls `NZ_myna_vs_starling_humboldt.R` and performs NOA between starlings and mynas in New Zealand, and makes **Figures S2.10 and S2.11**.

