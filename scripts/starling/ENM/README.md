# ENM

This folder performs ENM on the starling data. This folder is structured as follows:

```
├── native/
│   
├── nativeNZ/
│   
├── NZ/
│   
├── 02_starling_tune_biomod2_maxent.R
├── 03_starling_blockCV_ENM.R
├── 03b_starling_blockCV_ENM_projections.R
├── 04_starling_full_ENM.R
├── 05_starling_model_eval_proj.R
└── README.md
```

This folder contains three folders, each containing scripts which perform ENM on each dataset, namely, native, NZ, and native+NZ. In this folder, there are also scripts that are being called by scripts in the three folders (`native/`, `NZ/`, and `nativeNZ/`).

Within each folder, there are scripts number 01 to 05 or 06. The purpose of the scripts are as follows:

* `01_*` - these scripts create the spatial blocking structure for spatial cross-validation. It also converts the dataset into the format for BIOMOD2
* `02*` - tunes maxent parameters, by calling 02_starling_tune_biomod2_maxent.R in the parent folder (ENM folder)
* `03*` - performs ENM using the spatial block cross-validation approach, calls 03_starling_blockCV_ENM.R in the parent folder.
* `04*` - performs ENM on the entire dataset, calls 04_starling_full_ENM.R in the parent folder.
* `05*` - performs cross-evaluation in for the NZ and native models, calls 05_starling_model_eval_proj.R in the parent folder. In the nativeNZ folder, `05*` scripts were used to make plots of predicted ENM distributions in NZ and the native range.
* `06*` - makes future projections of starling distribution in New Zealand. Only performed on well-performing models. In the case of starlings, this was only performed on the NZ-only model and the nativeNZ model.




