# Protecting birds in protected areas: a multi-scale analysis of forest structure and species occurrence

### Bruna R. Amaral, Jeffrey W. Doser, Aaron Weed, Kate Miller, and Elise F. Zipkin

Publication on *Landscape Ecology*: [![DOI]()], [![PDF]()]

Zipkin Lab Code Archive: [https://zipkinlab.github.io/](https://zipkinlab.github.io/)

Zenodo: [https://zenodo.org/records/20291426](https://zenodo.org/records/20291426)

GitHub: [https://github.com/br-amaral/NPS_bird_copy](https://github.com/br-amaral/NPS_bird)

### Citation



### Abstract

<b> Context. </b> Protected areas are cornerstones of avian conservation, yet forest-interior bird communities continue to decline even within protected lands. The capacity of a protected area to sustain bird populations depends on both internal habitat quality and the surrounding landscape context, yet the relative importance of these scales remains poorly understood.

<b> Objectives. </b> We evaluate how forest structure at three nested spatial levels (stand, park, and region) influences the occurrence of forest-interior birds within protected areas. 

<b> Methods. </b> We analyzed 17 years (2006-2023) of point count data for 16 forest-bird species across eight National Parks in the Northeastern United States to determine the relationships between species occurrences and five forest structure variables, including the dominant spatial level for each. Forest structure variables were derived at stand, park, and region spatial levels using long-term park monitoring and national forest inventory data.

<b> Results. </b> Region-level forest variables were selected most frequently as the dominant level of effect (19 species-covariate combinations), followed by stand (17) and park (5) levels, with at least one regional forest variable influencing 75% of species. Overall, all but two species showed significant associations with forest structure variables. Park size had weak and inconsistent effects on occurrence, likely because the parks included in our study are all small. While these parks contain quality avian habitat, regional forest availability and connectivity may constrain which species persist within them.

<b> Conclusions. </b> Protected areas cannot conserve forest-interior birds in isolation; regional landscape context is an equally important determinant of species occurrences, especially for small protected areas. Management of the matrix habitat surrounding protected areas, such as restoring forest availability and improving connectivity, may yield greater conservation returns than management of protected areas alone. This can be a tractable strategy for practitioners in regions where expanding formal protection is constrained by competing land uses.

<i> Keywords: </i> forest-interior birds; multi-scale habitat selection; protected areas; forest structure; landscape context; occupancy modeling

--------------------------------------

This repository contains code to format bird survey and vegetation data collected by NETN NPS, format FIA vegetation data, fit multi-scale hierarchical occupancy models, and generate figures and predictions of bird occurrence across northeastern national parks. All code (numbered in order of execution), data, and outputs are provided.

### Folder structure:
- <b>[code](#code)</b>: R scripts numbered in execution order, organized into subfolders by task (bird data formatting, vegetation data formatting, and model fitting). Divided into:
    - <b>[format_veg_data](#format_veg_data)</b>:
    - <b>[format_bird_data](#format_bird_data)</b>: 
    - <b>[fit_model](#format_bird_data)</b>: 

- <b>[data](#data)</b>: all data used in the analysis, including processed outputs. Divided into:
    - <b>[ana_file](#ana_file)</b>:
    - <b>[park_raster](#park_raster)</b>: 
    - <b>[veg_maps](#veg_maps)</b>: 
    - <b>[NETN-forest](#NETN-forest)</b>: 
        - <b>[forest_csvs](#forest_csvs)</b>: 
        - <b>[src](#src)</b>: 
    - <b>[src](#src)</b>: raw input data from NETN bird surveys and vegetation monitoring.
        - <b>[original](#original)</b>: 
    - <b>[out](#out)</b>: processed intermediate and final data files used as model inputs.
    - <b>[model_res](#model_res)</b>: JAGS model output files per species and park.
    - <b>[FIA](#FIA)</b>: county-level forest inventory data.
        - <b>[out](#out)</b>:
        - <b>[processed](#processed)</b>: 
    - <b>[veg_kateaaron](#veg_kateaaron)</b>: forest plot-level vegetation data from NETN monitoring.

- <b>[models](#models)</b>:

- <b>[sbatch](#sbatch)</b>:

- <b>[figures](#figures)</b>:

### Files:

### code

##### format_bird_data

<b>[1_ImportData.R](code/format_bird_data/1_ImportData.R)</b>: imports and extracts NETN bird survey data.

&nbsp;&nbsp;<u>Input:</u>
- [data/src/original/NETN_2020](data/src/original/NETN_2020)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/NETNtib.rds](data/out/NETNtib.rds)
- [data/key_park.rds](data/key_park.rds)

<b>[2_create_data_files.R](code/format_bird_data/2_create_data_files.R)</b>: creates final data arrays with correct site, year, and occasion structure, and merges covariate values with bird data.

&nbsp;&nbsp;<u>Source:</u>
- <b>[format_data.R](code/format_bird_data/format_data.R)</b>: filters visits to auditory detections within 50 m, removes records with missing values in key columns.

&nbsp;&nbsp;<u>Input:</u>
- [data/out/NETNtib.rds](data/out/NETNtib.rds)
- [data/out/coun_covs.rds](data/out/coun_covs.rds)
- [data/out/park_covs.rds](data/out/park_covs.rds)
- [data/out/site_covs_fornofor_{radi_dist}m.rds](data/out/) or [data/out/site_covs_hardcon_{radi_dist}m.rds](data/out/)

&nbsp;&nbsp;<u>Output:</u>
- [data/y_dat8.rds](data/y_dat8.rds)
- [data/X.rds](data/X.rds)
- [data/nsite_pk.csv](data/nsite_pk.csv)

##### format_veg_data

<b>[NETN_forest_data_for_sites.R](code/format_veg_data/NETN_forest_data_for_sites.R)</b>: extracts forest plot-level covariates from NETN vegetation monitoring data.

&nbsp;&nbsp;<u>Input:</u>
- [data/veg_kateaaron/ForestNETN2024.zip](data/veg_kateaaron/ForestNETN2024.zip)
- [data/tree_sps_harcon.csv](data/tree_sps_harcon.csv)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds)

<b>[get_site_data_rad.R](code/format_veg_data/get_site_data_rad.R)</b>: links forest plots to bird survey sites within a 400 m radius and calculates inverse-distance-weighted mean covariate values per bird site.

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

<b>[get_park_data.R](code/format_veg_data/get_park_data.R)</b>: compiles park-level forest covariates.

&nbsp;&nbsp;<u>Input:</u>
- [data/out/for_plot_covs.rds](data/out/for_plot_covs.rds)
- [data/VAMA_sites.rds](data/VAMA_sites.rds)
- [data/HOFR_sites.rds](data/HOFR_sites.rds)
- [data/ELRO_sites.rds](data/ELRO_sites.rds)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/park_covs.rds](data/out/park_covs.rds)

<b>[get_coun_data.R](code/format_veg_data/get_coun_data.R)</b>: compiles county-level forest covariates from FIA data.

&nbsp;&nbsp;<u>Input:</u>
- [data/FIA/](data/FIA/)

&nbsp;&nbsp;<u>Output:</u>
- [data/out/coun_covs.rds](data/out/coun_covs.rds)

##### fit_model

<b>[back2d_covs_scales_2min_spscov.R](code/fit_model/back2d_covs_scales_2min_spscov.R)</b>: fits hierarchical JAGS model for each species and park combination.

&nbsp;&nbsp;<u>Input:</u>
- [data/y_dat8.rds](data/y_dat8.rds)
- [data/X.rds](data/X.rds)
- [data/out/nsite_pk.rds](data/out/nsite_pk.rds)
- [data/key_park.rds](data/key_park.rds)

&nbsp;&nbsp;<u>Output:</u>
- [data/model_res/jags_res_{sps}_{park}_run{run_number}.rds](data/model_res/)

<b>[run_step1_step2.R](code/fit_model/run_step1_step2.R)</b>: orchestrates sequential model fitting steps; submitted to HPC via `nps_source.sb`.

### data

##### data/out

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

##### data (root-level)

- [data/y_dat8.rds](data/y_dat8.rds): detection array (species × sites × years × occasions) used as model response
- [data/X.rds](data/X.rds): covariate matrix used as model predictors
- [data/nsite_pk.csv](data/nsite_pk.csv): number of sites per park (csv version)
- [data/key_park.rds](data/key_park.rds): key linking park codes to park names and metadata
- [data/tree_sps_harcon.csv](data/tree_sps_harcon.csv): lookup table classifying tree species as hardwood or conifer

##### data/model_res

- [data/model_res/jags_res_{sps}_{park}_run{run_number}.rds](data/model_res/): JAGS posterior samples for each species–park model run

### Assumptions and decisions

- Forest covariates for sites with no nearby forest plots are imputed as zero
- Parks removed from analysis: ACAD (too large), SAIR (open habitat, too different), ELRO (only one forest plot)
- 400 m radius used to link bird survey sites to forest plots
- Forest covariates averaged across all years with available data to account for panel rotation design in vegetation monitoring
