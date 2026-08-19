# Protecting birds in protected areas: a multi-scale analysis of forest structure and species occurrence

### Bruna R. Amaral, Jeffrey W. Doser, Aaron Weed, Kate Miller, and Elise F. Zipkin

&nbsp;&nbsp; Publication on *Landscape Ecology*: [![DOI]()], [![PDF]()]

&nbsp;&nbsp; Zipkin Lab Code Archive: [https://zipkinlab.github.io](https://zipkinlab.github.io/)

&nbsp;&nbsp; Zenodo: [https://zenodo.org/records/20291426](https://zenodo.org/records/20291426)

&nbsp;&nbsp; GitHub: [https://github.com/br-amaral/NPS_bird_copy](https://github.com/br-amaral/NPS_bird)

### Citation

Amaral, B. R., Doser, J. W., Weed, A., Miller, K., & Zipkin, E. F. (2026). Protecting birds in protected areas: A multi-scale analysis of forest structure and species occurrence. *Landscape Ecology* xx(xx): xx - xx.

### Abstract

**Context.** Protected areas are cornerstones of avian conservation, yet forest-interior bird communities continue to decline even within protected lands. The capacity of a protected area to sustain bird populations depends on both internal habitat quality and the surrounding landscape context, yet the relative importance of these scales remains poorly understood.

**Objectives.** We evaluate how forest structure at three nested spatial levels (stand, park, and region) influences the occurrence of forest-interior birds within protected areas. 

**Methods.** We analyzed 17 years (2006-2023) of point count data for 16 forest-bird species across eight National Parks in the Northeastern United States to determine the relationships between species occurrences and five forest structure variables, including the dominant spatial level for each. Forest structure variables were derived at stand, park, and region spatial levels using long-term park monitoring and national forest inventory data.

**Results.** Region-level forest variables were selected most frequently as the dominant level of effect (19 species-covariate combinations), followed by stand (17) and park (5) levels, with at least one regional forest variable influencing 75% of species. Overall, all but two species showed significant associations with forest structure variables. Park size had weak and inconsistent effects on occurrence, likely because the parks included in our study are all small. While these parks contain quality avian habitat, regional forest availability and connectivity may constrain which species persist within them.

**Conclusions.** Protected areas cannot conserve forest-interior birds in isolation; regional landscape context is an equally important determinant of species occurrences, especially for small protected areas. Management of the matrix habitat surrounding protected areas, such as restoring forest availability and improving connectivity, may yield greater conservation returns than management of protected areas alone. This can be a tractable strategy for practitioners in regions where expanding formal protection is constrained by competing land uses.

*Keywords:* forest-interior birds; multi-scale habitat selection; protected areas; forest structure; landscape context; occupancy modeling

--------------------------------------
## Repository

This repository contains code to format bird point count and forest survey data collected by NETN I&M NPS, format FIA vegetation data, fit multi-scale hierarchical occupancy models, and generate figures and predictions of bird occurrence across Northeastern National Parks. All code (numbered in order of execution), data, and outputs are provided.

### Folder structure:

- **[code](#code)**: R scripts numbered in execution order, organized into subfolders by task (forest data formatting, bird data formatting, and model fitting). Divided into:

  - **[format_veg_data](#format_veg_data)**: folder with code to extract and format the forest structure data for analysis.

  - **[format_bird_data](#format_bird_data)**: folder with code to format the bird data data for analysis.

  - **[fit_model](#fit_model)**: folder with code to merge forest and bird data, run analysis, and generate figures.
&nbsp;

- **[data](#data)**: all data used in the analysis, including processed outputs. Divided into:
  - **[ana_file](#ana_file)**: folder to save the processed species-specific bird data used in the model, the metadata about the model run (e.g. number of iterations and initial values).
  - **[park_raster](#park_raster)**: raster files of all parks to obtain area and location.
  - **[NETN-forest](#NETN-forest)**: forest data collected by the NPS within parks.
    - **[forest_csvs](#forest_csvs)**:
    - **[src](#src)**: 
  - **[src](#src)**: raw input data from NETN bird surveys and bird information (phylogeny, guild).
    - **[original](#original)**: original data files that have not been opened, just exported.
  - **[out](#out)**: placeholder folder for processed intermediate and final data files used as analysis inputs.
  - **[model_res](#model_res)**: JAGS model output files per species for steps 1 and 2.
  - **[FIA](#FIA)**: county-level forest inventory data obtained with rFIA R package.
    - **[out](#out)**: forest structure covariates calculated and used in the analysis.
&nbsp;
- **[models](#models)**: .txt model files with JAGS models for each species and step.
&nbsp;
- **[sbatch](#sbatch)**: .sb files to run analysis in the cluster
&nbsp;

- **[figures](#figures)**: folder to hold figures generated in the analysis.

### Files within folders:
Each R script includes a description of its goal, the files needed to run it (Input), the other code used within it (Source), and the files generated by it (Output). Scripts with numbers in their name (e.g., 1_xxxx.R) should be executed in numeric order, and without numbers do not have to be in a specific order.

### code/

#### format_veg_data/

**[NETN_forest_data_for_sites.R](./code/format_veg_data/NETN_forest_data_for_sites.R)**: extracts forest plot-level covariates from NETN vegetation monitoring data.

&nbsp;&nbsp;<u>Input:</u>
- [data/veg_kateaaron/ForestNETN2024.zip](data/veg_kateaaron/ForestNETN2024.zip)
- [data/tree_sps_harcon.csv](data/tree_sps_harcon.csv)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds)

**[get_site_data_rad.R](code/format_veg_data/get_site_data_rad.R)**: links forest plots to bird survey sites within a 400 m radius and calculates inverse-distance-weighted mean covariate values per bird site.

&nbsp;&nbsp;<u>Input:</u>
- [data/out/NETNtib.rds](data/out/NETNtib.rds)
- [data/key_park.rds](data/key_park.rds)
- [data/out/updated_for_cats.csv](data/out/updated_for_cats.csv)
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds)
- [data/out/key_bsite.rds](data/out/key_bsite.rds)
- [data/out/key_fsite.rds](data/out/key_fsite.rds)
- [data/out/park_site_UTM.rds](data/out/park_site_UTM.rds)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/site_covs_fornofor_{radi_dist}m.rds](data/out/)
- [data/out/neighbor_fornofor_{radi_dist}m.rds](data/out/)
- [data/out/site_covs_hardcon_{radi_dist}m.rds](data/out/)
- [data/out/neighbor_hardcon_{radi_dist}m.rds](data/out/)

**[get_park_data.R](code/format_veg_data/get_park_data.R)**: compiles park-level forest covariates.

&nbsp;&nbsp;<u>Input:</u>
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds)
- [data/VAMA_sites.rds](data/VAMA_sites.rds)
- [data/HOFR_sites.rds](data/HOFR_sites.rds)
- [data/ELRO_sites.rds](data/ELRO_sites.rds)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/park_covs.rds](data/out/park_covs.rds)

**[get_coun_data.R](code/format_veg_data/get_coun_data.R)**: compiles county-level forest covariates from FIA data.

&nbsp;&nbsp;<u>Input:</u>
- [data/FIA/](data/FIA/)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/coun_covs.rds](data/out/coun_covs.rds)

#### format_bird_data/

**[1_ImportData.R](code/format_bird_data/1_ImportData.R)**: imports and extracts NETN bird survey data.

&nbsp;&nbsp;<u>Input:</u>
- [data/src/original/NETN_2020](data/src/original/NETN_2020)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/NETNtib.rds](data/out/NETNtib.rds)
- [data/key_park.rds](data/key_park.rds)

**[2_create_data_files.R](code/format_bird_data/2_create_data_files.R)**: creates final data arrays with correct site, year, and occasion structure, and merges covariate values with bird data.

&nbsp;&nbsp;<u>Source:</u>
- **[format_data.R](code/format_bird_data/format_data.R)**: filters visits to auditory detections within 50 m, removes records with missing values in key columns.

&nbsp;&nbsp;<u>Input:</u>
- [data/out/NETNtib.rds](data/out/NETNtib.rds)
- [data/out/coun_covs.rds](data/out/coun_covs.rds)
- [data/out/park_covs.rds](data/out/park_covs.rds)
- [data/out/site_covs_fornofor_{radi_dist}m.rds](data/out/) or [data/out/site_covs_hardcon_{radi_dist}m.rds](data/out/)

&nbsp;&nbsp;**Output:**
- [data/y_dat8.rds](data/y_dat8.rds)
- [data/X.rds](data/X.rds)
- [data/nsite_pk.csv](data/nsite_pk.csv)

#### fit_model/

**[back2d_covs_scales_2min_spscov.R](code/fit_model/back2d_covs_scales_2min_spscov.R)**: fits hierarchical JAGS model for each species and park combination.

&nbsp;&nbsp;<u>Input:</u>
- [data/y_dat8.rds](data/y_dat8.rds)
- [data/X.rds](data/X.rds)
- [data/out/nsite_pk.rds](data/out/nsite_pk.rds)
- [data/key_park.rds](data/key_park.rds)

&nbsp;&nbsp;<u>Output:</u>
- [data/model_res/jags_res_{sps}_{park}_run{run_number}.rds](data/model_res/)

**[run_step1_step2.R](code/fit_model/run_step1_step2.R)**: orchestrates sequential model fitting steps; submitted to HPC via `nps_source.sb`.

### data/

#### data (root-level)

- [data/y_dat8.rds](data/y_dat8.rds): detection array (species × sites × years × occasions) used as model response
- [data/X.rds](data/X.rds): covariate matrix used as model predictors
- [data/nsite_pk.csv](data/nsite_pk.csv): number of sites per park (csv version)
- [data/key_park.rds](data/key_park.rds): key linking park codes to park names and metadata
- [data/tree_sps_harcon.csv](data/tree_sps_harcon.csv): lookup table classifying tree species as hardwood or conifer

#### data/out/

- [data/out/NETNtib.rds](data/out/NETNtib.rds): tibble with imported and extracted NETN bird survey data
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds): forest plot-level covariates from NETN vegetation monitoring
- [data/out/site_covs_fornofor_{radi_dist}m.rds](data/out/): site-level forest/non-forest covariate values averaged within a given radius
- [data/out/site_covs_hardcon_{radi_dist}m.rds](data/out/): site-level hardwood/conifer covariate values averaged within a given radius
- [data/out/neighbor_fornofor_{radi_dist}m.rds](data/out/): neighbor forest plot information for forest/non-forest classification
- [data/out/neighbor_hardcon_{radi_dist}m.rds](data/out/): neighbor forest plot information for hardwood/conifer classification
- [data/out/park_covs.rds](data/out/park_covs.rds): park-level forest covariates
- [data/out/coun_covs.rds](data/out/coun_covs.rds): county-level forest covariates from FIA
- [data/out/park_site_UTM.rds](data/out/park_site_UTM.rds): UTM coordinates for park bird survey sites
- [data/out/key_bsite.rds](data/out/key_bsite.rds): key file linking bird site IDs to park and location info
- [data/out/key_fsite.rds](data/out/key_fsite.rds): key file linking forest plot IDs to park and location info
- [data/out/updated_for_cats.csv](data/out/updated_for_cats.csv): updated forest category classifications for sites
- [data/out/nsite_pk.rds](data/out/nsite_pk.rds): number of sites per park

#### data/model_res

- [data/model_res/jags_res_{sps}_{park}_run{run_number}.rds](data/model_res/): JAGS posterior samples for each species–park model run

