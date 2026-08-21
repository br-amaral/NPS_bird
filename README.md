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

This repository contains code to format bird point count and forest survey data collected by the <a href="https://www.nps.gov/im/netn/index.htm" target="_blank" rel="noopener noreferrer">Northeast Temperate Network (NETN)</a> regional monitoring groups in the National Park Service (NPS) Inventory & Monitoring (I&M) program, format <a href="https://research.fs.usda.gov/programs/fia" target="_blank" rel="noopener noreferrer">U.S. Forest Service Forest Inventory and Analysis (FIA)</a>, fit multi-scale hierarchical occupancy models, and generate figures and predictions of bird occurrence across eight Northeastern National Parks. All R code files, data, and outputs are provided.

### Folder structure:
### [code/](#code)
&nbsp;&nbsp;&nbsp;&nbsp;R scripts in execution order, organized into subfolders by task (forest data formatting, bird data formatting, and model fitting):

  - **[format_bird_data/](#format_bird_data)**: folder with code to format the bird data for analysis.
    - **[1_import_data.R](./code/format_bird_data/1_import_data.R)**: imports and extracts NETN bird survey data.
    - **[6_create_data_files.R](./code/format_bird_data/6_create_data_files.R)**: creates final data arrays with correct site, year, and occasion structure, and merges covariate values with bird data.
      - **[format_data.R](./code/format_bird_data/format_data.R)**: filters visits to auditory detections within 50 m, removes records with missing values in key columns.

  - **[format_veg_data/](#format_veg_data)**: folder with code to extract and format the forest structure data for analysis.
    - **[2_NETN_forest_data_for_sites.R](./code/format_veg_data/2_NETN_forest_data_for_sites.R)**: extracts forest plot-level covariates from NETN vegetation monitoring data.
    - **[3_get_site_data_rad.R](./code/format_veg_data/3_get_site_data_rad.R)**: links forest plots to bird survey sites within a 400 m radius and calculates inverse-distance-weighted mean covariate values per bird site.
    - **[4_get_park_data.R](./code/format_veg_data/4_get_park_data.R)**: compiles park-level forest covariates.
    - **[5_get_coun_data.R](./code/format_veg_data/5_get_coun_data.R)**: compiles county-level forest covariates from FIA data.

  - **[fit_model/](#fit_model)**: folder with code to merge forest and bird data, run analysis, and generate figures.
    - **[7_run_step1_step2.R](./code/fit_model/7_run_step1_step2.R)**: orchestrates model fitting of step 1 OR step 2 (step 2 depends on step 1 results); sources:
      - **[back2d_covs_scales_2min_spscov.R](./code/fit_model/back2d_covs_scales_2min_spscov.R)**: fits a hierarchical JAGS model for each species for step 1 analysis according to the [mod_key.csv](./code/fit_model/mod_key.csv) species key.
      - **[step2_analysis.R](./code/fit_model/step2_analysis.R)**: fits a hierarchical JAGS model for each species for step 2 analysis, and it only works after running step 1 and entering the file names in the [mod_key.csv](./code/fit_model/mod_key.csv) species key.

### [data/](#data)
&nbsp;&nbsp;&nbsp;&nbsp; All data used in the analysis, including processed outputs. Divided into:
  - **[ana_file/](#ana_file)**: folder to save the processed species-specific bird data used in the model, the metadata about the model run (e.g., number of iterations and initial values).
  - **[park_raster/](#park_raster)**: raster files of all parks to obtain area and location.
  - **[NETN-forest/](#NETN-forest)**: forest data collected by the NPS within parks.
    - **[forest_csvs/](#forest_csvs)**:
    - **[src/](#src)**: 
  - **[src/](#src)**: raw input data from NETN bird surveys and bird information (phylogeny, guild).
    - **[original/](#original)**: original data files that have not been opened, just exported.
  - **[out/](#out)**: placeholder folder for processed intermediate and final data files used as analysis inputs.
  - **[model_res/](#model_res)**: JAGS model output files per species for steps 1 and 2.
  - **[FIA/](#FIA)**: county-level forest inventory data obtained with rFIA R package.
    - **[out/](#out)**: forest structure covariates calculated and used in the analysis.
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

- [data/src/original/NETN_2020](./data/src/original/NETN_2020): tibble with imported and extracted NETN bird survey data.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/out/NETNtib.rds](./data/out/NETNtib.rds): tibble with imported and extracted NETN bird survey data.
- [data/key_park.rds](./data/key_park.rds): file with all unique park names.

**[6_create_data_files.R](./code/format_bird_data/6_create_data_files.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Source:*

- [format_data.R](./code/format_bird_data/format_data.R): filtering and select bird observations for analysis.

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/out/NETNtib.rds](./data/out/NETNtib.rds): tibble with imported and extracted NETN bird survey data (sourced from [format_data.R](./code/format_bird_data/format_data.R)).
- [data/out/coun_covs.rds](./data/out/coun_covs.rds): county-level forest covariates from FIA.
- [data/out/park_covs.rds](./data/out/park_covs.rds): tibble with park-level forest variables.
- [data/out/site_covs_fornofor_{radi_dist}m.rds](./data/out/site_covs_fornofor_{radi_dist}m.rds): site-level forest covariates data.
- [data/park_raster/{park_size[park]}_pb.rds](./data/park_raster/{park_size[park]}_pb.rds): shapefiles of park area to calculate area.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/y_dat8.rds](./data/y_dat8.rds): birds data for each occasion, with park, species, and site indexes.
- [data/X.rds](./data/X.rds): forest variables for all scales for each occasion, same dim() as y_dat8.rds.
- [data/nsite_pk.rds](./data/nsite_pk.rds): number of sites per park (sourced from [format_data.R](./code/format_bird_data/format_data.R))

### code/format_veg_data/

**[2_NETN_forest_data_for_sites.R](./code/format_veg_data/2_NETN_forest_data_for_sites.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/veg_kateaaron/ForestNETN2024.zip](./data/veg_kateaaron/ForestNETN2024.zip): folder with all the forest data for the parks.
- [data/tree_sps_harcon.csv](./data/tree_sps_harcon.csv): list with all tree genera recorded in the park, classified as conifer or hardwood.

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/out/for_plot_covs.rds](./data/out/for_plot_covs.rds): forest covariates for all forest plots.

**[3_get_site_data_rad.R](./code/format_veg_data/3_get_site_data_rad.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/key_park.rds](./data/key_park.rds ): file with all park names.
- [data/ELRO_sites.rds](./ELRO_sites.rds): file to get forest plot names for ELRO that are not named after ROVA.
- [data/HOFR_sites.rds](./data/HOFR_sites.rds): file to get forest plot names for HOFR that are not named after ROVA.
- [data/VAMA_sites.rds](./data/VAMA_sites.rds): file to get forest plot names for VAMA that are not named after ROVA.
- [data/out/NETNtib.rds](./data/out/NETNtib.rds): tibble with imported and extracted NETN bird survey data.
- [data/out/updated_for_cats.csv](./data/out/updated_for_cats.csv): vegetation types/categories of the parks (to be classified as forest/not forest).
- [data/out/for_plot_covs.rds](./data/out/for_plot_covs.rds): forest covariates for all forest plots created by NETN_forest_data_for_sites.R.
- [data/out/key_bsite.rds](./data/out/key_bsite.rds): forest type classification for each bird site.
- [data/out/key_fsite.rds](./data/out/key_fsite.rds): forest type classification for each forest site.
- [data/out/park_site_UTM.rds](./data/out/park_site_UTM.rds): UTM coordinates of sites within each park.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/out/site_covs_fornofor_400m.rds](./data/out/site_covs_fornofor_{radi_dist}m.rds): forest covariates for each bird site according to the weighted mean by distance of the closest 5 forest plots.
- [data/out/neighbor_fornofor_400m.rds](./data/out/neighbor_fornofor_{radi_dist}m.rds): who is whose neighbor.

**[4_get_park_data.R](./code/format_veg_data/4_get_park_data.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/out/for_plot_covs.rds](./data/out/for_plot_covs.rds): forest covariates for all forest plots created by NETN_forest_data_for_sites.R.
- [data/ELRO_sites.rds](./ELRO_sites.rds): file to get forest plot names for ELRO that are not named after ROVA.
- [data/HOFR_sites.rds](./data/HOFR_sites.rds): file to get forest plot names for HOFR that are not named after ROVA.
- [data/VAMA_sites.rds](./data/VAMA_sites.rds): file to get forest plot names for VAMA that are not named after ROVA.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/out/park_covs.rds](./data/out/park_covs.rds): tibble with park-level forest variables.

**[5_get_coun_data.R](./code/format_veg_data/5_get_coun_data.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/FIA/](./data/FIA/): folder with FIA data downloaded with the code using the rFIA package
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/out/coun_covs.rds](./data/out/coun_covs.rds): county-level forest covariates from FIA

### code/fit_model/

**[6_run_step1_step2.R](./code/fit_model/6_run_step1_step2.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [models/mod_all_covs.txt](./models/mod_all_covs.txt): model files with all covariates for step 1.
- [code/fit_model/mod_key.csv](./code/fit_model/mod_key.csv): table with a key to run models that includes: species names, step, result file name from step 1 used in step 2 analysis, selected scales file name for step 2 analysis.
- [data/model_res/{output_selected_scale}.rds](./data/model_res/{tib_loop$select}.rds): if step 2 of analysis, this will link to which covariates and scales were selected at step 1 ([data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds](./data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds)).
  
**[back2d_covs_scales_2min_spscov.R](./code/fit_model/back2d_covs_scales_2min_spscov.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/y_dat8.rds](./data/y_dat8.rds): birds data for each occasion, with park, species, and site indexes.
- [data/X.rds](./data/X.rds): tibble with covariate data.
- [data/out/nsite_pk.rds](./data/out/nsite_pk.rds): vector with number of sites in each park.
- [data/key_park.rds](./data/key_park.rds): vector of all parks being analyzed.
  
&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds](./data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds): jags data for species for analysis.
- [data/ana_file/{sps}_step1_Z_{date_step1}.rds](./data/ana_file/{sps}_step1_Z_{date_step1}.rds): initial values for analysis.
- [data/ana_file/{sps_loop}_step{step_numb}_model_{date_step1}.txt](./data/ana_file/{sps_loop}_step{step_numb}_model_{date_step1}.txt): model file for species.
- [data/model_res/{sps_loop}_step{step_numb}_output_{date_step1}{index_run}.rds](data/model_res/{sps_loop}_step{step_numb}_output_{date_step1}{index_run}.rds): model results from jags model.
- [data/ana_file/{sps_loop}_step{step_numb}_metadata_{date_step1}_int.txt](./data/ana_file/{sps_loop}_step{step_numb}_metadata_{date_step1}_int.txt): metadata for the analysis (species, covariates, iterations, step, date, etc.).
- [data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds](./data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds): file with which scales were selected as most influential.

**[step2_analysis.R](./code/fit_model/step2_analysis.R)**:

&nbsp;&nbsp;&nbsp;&nbsp;*Input:*

- [data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds](./data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds): jags data for species for analysis.
- [data/ana_file/{sps}_step1_Z_{date_step1}.rds](./data/ana_file/{sps}_step1_Z_{date_step1}.rds): initial values for analysis.
- [models/mod_all_covs_hyper.txt](./models/mod_all_covs_hyper.txt): hyperparameter section of model for step 2
- [models/mod_all_covs_det.txt](./models/mod_all_covs_det.txt): detection section of model for step 2

&nbsp;&nbsp;&nbsp;&nbsp;*Output:*

- [models/{sps_loop}_step2_model{mod_nam_par}_scales{mod_nam_sca}_{date_step1}.txt](./models/{sps_loop}_step2_model{mod_nam_par}_scales{mod_nam_sca}_{date_step1}.txt): model for step 2 analysis containing only selected covariates and scales according to step 1.
- [data/model_res/{sps}_step{step_numb}_output_{date_step2}_new.rds](./data/model_res/{sps}_step{step_numb}_output_{date_step2}_new.rds): step 2 results output file.
- [data/ana_file/{sps}_step{step_numb}_metadata_{date_step2}.txt](./data/ana_file/{sps}_step{step_numb}_metadata_{date_step2}.txt): metadata of step 2 model run for a species.

### models/
        
### figures/
