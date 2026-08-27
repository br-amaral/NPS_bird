# Protecting birds in protected areas: a multi-scale analysis of forest structure and species occurrence

### Bruna R. Amaral, Jeffrey W. Doser, Aaron Weed, Kate Miller, and Elise F. Zipkin

&nbsp;&nbsp; Publication on *Landscape Ecology*: [![DOI]()], [![PDF]()]

&nbsp;&nbsp; Zipkin Lab Code Archive: [https://zipkinlab.github.io](https://zipkinlab.github.io/)

&nbsp;&nbsp; Zenodo: [https://zenodo.org/records/20291426](https://zenodo.org/records/20291426)

&nbsp;&nbsp; GitHub: [https://github.com/br-amaral/NPS_bird](https://github.com/br-amaral/NPS_bird)

### Citation

Amaral, B. R.; Doser, J. W.; Weed, A.; Miller, K.; & Zipkin, E. F. (2026). Protecting birds in protected areas: A multi-scale analysis of forest structure and species occurrence. *Landscape Ecology* xx(xx): xx - xx.

### Abstract
**Context.** Protected areas are cornerstones of avian conservation, yet forest-associated bird communities continue to decline even within protected lands. The capacity of a protected area to sustain bird populations depends on both internal habitat quality and the surrounding landscape context, yet the relative importance of these scales remains poorly understood.

**Objectives.** We evaluate how forest structure at three nested spatial scales (local, park, and county) influences the occurrence of forest-associated birds within protected areas.

**Methods.** We analyzed 17 years (2006-2023) of point count data for 16 forest-bird species across eight National Parks in the Northeastern United States to determine the relationships between species occurrences and five forest-structure variables, including the dominant spatial scale for each. Forest-structure variables were derived at local, park, and county spatial scales using long-term park monitoring and national forest inventory data.

**Results.** County-scale forest variables were selected most frequently as the dominant scale of effect (19 species-covariate combinations), followed by local (16) and park (5) scales, with at least one county-scale forest variable influencing 75% of species. Overall, all but two species showed significant associations with forest structure variables. Park size had weak and inconsistent effects on occurrence, likely because the parks included in our study are all small. While these parks contain quality avian habitat, county-scale forest availability and connectivity may constrain which species persist within them.

**Conclusions.** Protected areas cannot conserve forest-associated birds in isolation; regional landscape context at the county-scale is an equally important determinant of species occurrences, especially for small, protected areas. Management of the matrix habitat surrounding protected areas, such as restoring forest availability and improving connectivity, may yield greater conservation returns than management of protected areas alone. This can be a tractable strategy for practitioners in regions where expanding formal protection is constrained by competing land uses.

*Keywords:* forest-interior birds; multi-scale habitat selection; protected areas; forest structure; landscape context; occupancy modeling

--------------------------------------
## Repository

This repository contains code to format bird point count and forest survey data collected by the <a href="https://www.nps.gov/im/netn/index.htm" target="_blank" rel="noopener noreferrer">Northeast Temperate Network (NETN)</a> regional monitoring groups in the National Park Service (NPS) Inventory & Monitoring (I&M) program, format <a href="https://research.fs.usda.gov/programs/fia" target="_blank" rel="noopener noreferrer">U.S. Forest Service Forest Inventory and Analysis (FIA)</a>, fit multi-scale hierarchical occupancy models, and generate figures and predictions of bird occurrence across eight Northeastern National Parks. All R code files, data, and outputs are provided. The data folder is present only in the <a href="https://" target="_blank" rel="noopener noreferrer">Zenodo Repository</a>.

### Folder structure:
### [code/](#code)
&nbsp;&nbsp;&nbsp;&nbsp; R scripts in execution order, organized into subfolders by task (forest data formatting, bird data formatting, and model fitting):

  - **[format_bird_data/](#format_bird_data)**: folder with code to format the bird data for analysis.
    - **[1_import_data.R](./code/format_bird_data/1_import_data.R)**: imports and extracts NETN bird survey data.
    - **[6_create_data_files.R](./code/format_bird_data/6_create_data_files.R)**: creates final data arrays with correct site, year, and occasion structure, and merges covariate values with bird data; sources:
      - **[format_data.R](./code/format_bird_data/format_data.R)**: filters visits to auditory detections within 50 m, removes records with missing values in key columns.

  - **[format_veg_data/](#format_veg_data)**: folder with code to extract and format the forest structure data for analysis.
    - **[2_NETN_forest_data_for_sites.R](./code/format_veg_data/2_NETN_forest_data_for_sites.R)**: extracts forest plot-level covariates from NETN vegetation monitoring data.
    - **[3_get_site_data_rad.R](./code/format_veg_data/3_get_site_data_rad.R)**: links forest plots to bird survey sites within a 400 m radius and calculates inverse-distance-weighted mean covariate values per bird site.
    - **[4_get_park_data.R](./code/format_veg_data/4_get_park_data.R)**: compiles park-level forest covariates.
    - **[5_get_coun_data.R](./code/format_veg_data/5_get_coun_data.r)**: compiles county-level forest covariates from FIA data.

  - **[fit_model/](#fit_model)**: folder with code to merge forest and bird data, run analysis, and generate figures.
    - **[7_run_step1_step2.R](./code/fit_model/7_run_step1_step2.R)**: orchestrates model fitting of step 1 OR step 2 (step 2 depends on step 1 results); sources:
      - **[back2d_covs_scales_2min_spscov.R](./code/fit_model/back2d_covs_scales_2min_spscov.R)**: fits a hierarchical JAGS model for each species for step 1 analysis according to the [mod_key.csv](./code/fit_model/mod_key.csv) species key.
      - **[step2_analysis.R](./code/fit_model/step2_analysis.R)**: fits a hierarchical JAGS model for each species for step 2 analysis, and it only works after running step 1 and entering the file names in the [mod_key.csv](./code/fit_model/mod_key.csv) species key.
    - **[map.R](./code/fit_model/map.r)**: code to generate map of the study area displayed on figure 1.
    - **[nlcd_map.R](./code/fit_model/nlcd_map.r)**: code to generate figure S4, which is the NLCD landcover map for the study region.
    - **[8_coef_extract.R](./code/fit_model/8_coef_extract.r)**: code to get coefficients and make figures 2, 3, and 5.
    - **[9_source_x_min_max.R](code/fit_model/9_source_x_min_max.r)**: source x_min_max.R for all species and parks:
      - **[x_min_max.R](./code/fit_model/x_min_max.r)**: get covariate value range for each site and for sites where a sps was present.
    - **[10_pred_marg_plot.R](./code/fit_model/10_pred_marg_plot.r)**: code to make occurrence predictions and figure 4.

### data/
&nbsp;&nbsp;&nbsp;&nbsp; All data used in the analysis, including processed outputs. The data folder is at the <a href="https://" target="_blank" rel="noopener noreferrer">Zenodo Repository</a>. Folder divided into:
  - **ana_file/**: folder to save the processed species-specific bird data used in the model, and the metadata about the model run (e.g., number of iterations and initial values).
  - **park_raster/**: raster files of all parks to obtain area and location.
  - **NETN-forest/**: forest data collected by the NPS within parks.
    - **forest_csvs/**:
    - **src/**: 
  - **src/**: raw input data from NETN bird surveys and bird information (phylogeny, guild).
    - **original/**: original data files that have not been opened, just exported.
  - **out/**: placeholder folder for processed intermediate and final data files used as analysis inputs.
  - **model_res/**: JAGS model output files per species for steps 1 and 2.
  - **FIA/**: county-level forest inventory data obtained with the rFIA R package.
    - **out/**: forest structure covariates calculated and used in the analysis.
### [models/](#models)
&nbsp;&nbsp;&nbsp;&nbsp;.txt model files with JAGS models for each species and step.
### [figures/](#figures)
&nbsp;&nbsp;&nbsp;&nbsp;folder to hold figures generated in the analysis.

-----------------------------------------------
### Files with complete path and where they are created, sourced, and loaded:

Each R script includes a description of its goal, the files needed to run it (Input), the other code used within it (Source), and the files generated by it (Output). Scripts with numbers in their name (e.g., 1_xxxx.R) should be executed in numeric order, and those without numbers are executed within the numbered scripts. Curly braces denote variable values filled in at runtime, e.g., {sps} = 4-letter species code.

### code/format_bird_data/

**[1_import_data.R](./code/format_bird_data/1_import_data.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/src/original/NETN_2023</u>: tibble with imported and extracted NETN bird survey data.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/NETNtib.rds</u>: tibble with imported and extracted NETN bird survey data.
- <u>data/key_park.rds</u>: file with all unique park names.

**[6_create_data_files.R](./code/format_bird_data/6_create_data_files.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Source:*

- [format_data.R](./code/format_bird_data/format_data.R): filtering and select bird observations for analysis.

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/out/NETNtib.rds</u>: tibble with imported and extracted NETN bird survey data (sourced from [format_data.R](./code/format_bird_data/format_data.R)).
- <u>data/out/coun_covs.rds</u>: county-level forest covariates from FIA.
- <u>data/out/park_covs.rds</u>: tibble with park-level forest variables.
- <u>data/out/site_covs_fornofor_400m.rds</u>: site-level forest covariates data.
- <u>data/park_raster/park_size{park}_pb.rds</u>: shapefiles of park area to calculate area.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/y_dat8.rds</u>: bird data for each occasion, with park, species, and site indexes.
- <u>data/X.rds</u>: forest variables for all scales for each occasion, same dim() as y_dat8.rds.
- <u>data/out/nsite_pk.rds</u>: number of sites per park (sourced from [format_data.R](./code/format_bird_data/format_data.R))

### code/format_veg_data/

**[2_NETN_forest_data_for_sites.R](./code/format_veg_data/2_NETN_forest_data_for_sites.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/veg_kateaaron/ForestNETN2024.zip</u>: folder with all the forest data for the parks.
- <u>data/tree_sps_harcon.csv</u>: list with all tree genera recorded in the park, classified as conifer or hardwood.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/for_plot_covs.rds</u>: forest covariates for all forest plots.

**[3_get_site_data_rad.R](./code/format_veg_data/3_get_site_data_rad.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/key_park.rds</u>: file with all park names.
- <u>data/ELRO_sites.rds</u>: file to get forest plot names for ELRO that are not named after ROVA.
- <u>data/HOFR_sites.rds</u>: file to get forest plot names for HOFR that are not named after ROVA.
- <u>data/VAMA_sites.rds</u>: file to get forest plot names for VAMA that are not named after ROVA.
- <u>data/out/NETNtib.rds</u>: tibble with imported and extracted NETN bird survey data.
- <u>data/out/updated_for_cats.csv</u>: vegetation types/categories of the parks (to be classified as forest/not forest).
- <u>data/out/for_plot_covs.rds</u>: forest covariates for all forest plots created by NETN_forest_data_for_sites.R.
- <u>data/out/key_bsite.rds</u>: forest type classification for each bird site.
- <u>data/out/key_fsite.rds</u>: forest type classification for each forest site.
- <u>data/out/park_site_UTM.rds</u>: UTM coordinates of sites within each park.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/site_covs_fornofor_400m.rds</u>: forest covariates for each bird site according to the weighted mean by distance of the closest 5 forest plots.
- <u>data/out/neighbor_fornofor_400m.rds</u>: who is whose neighbor.

**[4_get_park_data.R](./code/format_veg_data/4_get_park_data.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/out/for_plot_covs.rds</u>: forest covariates for all forest plots created by NETN_forest_data_for_sites.R.
- <u>data/ELRO_sites.rds</u>: file to get forest plot names for ELRO that are not named after ROVA.
- <u>data/HOFR_sites.rds</u>: file to get forest plot names for HOFR that are not named after ROVA.
- <u>data/VAMA_sites.rds</u>: file to get forest plot names for VAMA that are not named after ROVA.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/park_covs.rds</u>: tibble with park-level forest variables.

**[5_get_coun_data.R](./code/format_veg_data/5_get_coun_data.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/FIA/</u>: folder with FIA data downloaded with the code using the rFIA package
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/coun_covs.rds</u>: county-level forest covariates from FIA

### code/fit_model/

**[7_run_step1_step2.R](./code/fit_model/7_run_step1_step2.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [models/mod_all_covs.txt](./models/mod_all_covs.txt): model files with all covariates for step 1.
- [code/fit_model/mod_key.csv](./code/fit_model/mod_key.csv): table with a key to run models that includes: species names, step, result file name from step 1 used in step 2 analysis, selected scales file name for step 2 analysis.
- <u>data/model_res/{output_selected_scale}.rds</u>: if in step 2 of the analysis, this will link to which covariates and scales were selected at step 1 (<u>data/model_res/{file_name2}_{quantile_name}_SCA_SEL_PARS.rds</u>).
  
**[back2d_covs_scales_2min_spscov.R](./code/fit_model/back2d_covs_scales_2min_spscov.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/y_dat8.rds</u>: bird data for each occasion, with park, species, and site indexes.
- <u>data/X.rds</u>: tibble with covariate data.
- <u>data/out/nsite_pk.rds</u>: vector with number of sites in each park.
- <u>data/key_park.rds</u>: vector of all parks being analyzed.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/ana_file/{species}_step{step_number}_jagsdata_{date_step1}.rds</u>: jags data for species for analysis.
- <u>data/ana_file/{species}_step1_Z_{date_step1}.rds</u>: initial values for analysis.
- <u>data/ana_file/{species}_step{step_number}_model_{date_step1}.txt</u>: model file for species.
- <u>data/model_res/{species}_step{step_number}_output_{date_step1}{index_run}.rds</u>: model results from jags model.
- <u>data/ana_file/{species}_step{step_number}_metadata_{date_step1}_int.txt</u>: metadata for the analysis (species, covariates, iterations, step, date, etc.).
- <u>data/model_res/{file_name2}_{quantile_name}_SCA_SEL_PARS.rds</u>: file with which scales were selected as most influential.

**[step2_analysis.R](./code/fit_model/step2_analysis.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/ana_file/{species}_step{step_number}_jagsdata_{date_step1}.rds</u>: jags data for species for analysis.
- <u>data/ana_file/{species}_step1_Z_{date_step1}.rds</u>: initial values for analysis.
- [models/mod_all_covs_hyper.txt](./models/mod_all_covs_hyper.txt): hyperparameter section of model for step 2
- [models/mod_all_covs_det.txt](./models/mod_all_covs_det.txt): detection section of model for step 2

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>models/{species}_step2_model{model_parameters}_scales{model_name_scale}_{date_step1}.txt</u>: model for step 2 analysis containing only selected covariates and scales according to step 1.
- <u>data/model_res/{sps}_step{step_number}_output_{date_step2}_new.rds</u>: step 2 results output file.
- <u>data/ana_file/{sps}_step{step_number}_metadata_{date_step2}.txt</u>: metadata of step 2 model run for a species.

**[8_coef_extract.R](./code/fit_model/8_coef_extract.r)**

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [code/fit_model/mod_key.csv](./code/fit_model/mod_key.csv): table with the path to all model results.
- <u>data/model_res/species}_step{step_number}_output_{date}run{run_number}</u>: mcmc samples for a species of step 1.
- <u>data/model_res/{species}_step{step_number}_output_{date}run{run_number}_25_75_SCA_SEL_PARS</u>: mcmc samples for a species of step 2.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/coefs_sps_sca.rds</u>: table with all the beta coefficient estimates with their scales.

**[9_source_x_min_max.R](code/fit_model/9_source_x_min_max.r)**

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [models/mod_all_covs.txt](./models/mod_all_covs.txt): model files with all covariates for step 1.
- [code/fit_model/mod_key.csv](./code/fit_model/mod_key.csv): table with a key to run models that includes: species names, step, result file name from step 1 used in step 2 analysis, selected scales file name for step 2 analysis.

**[x_min_max.R](./code/fit_model/x_min_max.r)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- <u>data/y_dat8.rds</u>: tibble with bird data.
- <u>data/X.rds</u>: tibble with covariate data.
- <u>data/out/nsite_pk.rds</u>: vector with number of sites in each park.
- <u>data/src/key_park.rds</u>: vector of all parks being analyzed.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- <u>data/out/X_sites_{species}</u>: covariate values for each bird site.
- <u>data/out/X_vals_{species}</u>: covariate values for each bird site where a species was detected.

**[10_pred_marg_plot.R](./code/fit_model/10_pred_marg_plot.r)**

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [code/fit_model/mod_key.csv](./code/fit_model/mod_key.csv): table with a key to run models that includes: species names, step, result file name from step 1 used in step 2 analysis, selected scales file name for step 2 analysis.
- <u>data/out/coefs_sps_sca.rds</u>: table with all the beta coefficient estimates with their scales.
- <u>data/model_res/{species}_step{step_number}output{date_step1}{index_run}.rds</u>: model results from jags model from step 2.
- <u>data/out/X_vals_{species}.rds</u>: covariate prediction range for each species
- <u>data/X.rds</u>: tibble with covariates values.

### models/
        
### figures/

## Software versions:
**JAGS:** 4.3.2-foss-2023a

**R:** 4.3.2-gfbf-2023a

**R Packages:**

- AHMbook 0.2.9
- BayesPostEst 0.4.0
- broom 1.0.7
- conflicted 1.2.0
- dplyr 1.1.4
- forcats 1.0.0
- forestNETN 1.04
- freshr 1.0.2
- ggh4x 0.3.1
- ggplot2 4.0.1
- glue 1.8.0
- here 1.0.2
- hms 1.1.3
- jagsUI 1.6.2
- lubridate 1.9.4
- MCMCvis 0.16.3
- microViz 0.13.1
- modelr 0.1.11
- NCRNbirds 0.6.5
- rjags 4-16
- scales 1.4.0
- stringr 1.5.1
- tidyr 1.3.1
- tidyverse 2.0.0
- fs 1.6.3
- knitr 1.45
- tibble 3.2.1
- ggnewscale 0.5.2
- raster 3.6-32
- reshape2 1.4.5
- rFIA 1.1.4
- sf 1.1-2
- sp 2.2-3
- splitstackshape 1.4.8.1
- svglite 2.2.2
- terra 1.9-34
- tigris 2.2.1
- tidyterra 1.2.0
- tidybayes 3.0.7
- viridis 0.6.5

&nbsp;
# TO DO
## manually compiled files
- models/mod_all_covs.txt
- models/mod_all_covs_hyper.txt
- models/mod_all_covs_det.txt
- mod_key file
  
- data/ELRO_sites.rds, data/HOFR_sites.rds, data/VAMA_sites.rds
- data/out/key_bsite.rds, data/out/key_fsite.rds
- data/out/park_site_UTM.rds
- ata/out/updated_for_cats.csv

## output + model 'secondary files'
- data/metadata
- data/JAGS MCMC sample files
- data/z values
- data/sca_sel_sps

## Original data
- data/src/original/NETN_2023
- data/veg_kateaaron/ForestNETN2024.zip
- data/FIA/
- data/tree_sps_harcon.csv 


