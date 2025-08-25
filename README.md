# ---------------------- my repository log file ---------------------

DOOOOO(ing):



# --------------------------------------------------------------------
TODO: list
(   ) make sure all files are created in a line and are in the proper folder from one script to the other:
-       data/out/key_bsite.rds
-       data/out/key_fsite.rds
-       data/out/updated_for_cats.csv
-       data/out/nsite_pk.rds (check 2_create_data_file: data/nsite_pk.csv)



# --------------------------------------------------------------------
Assumptions/decisions:

- add covariate value inputation - some site level have no forest covariates; right now im adding zero

- parks removed: acadia is too big
                 sair is too different (open areas)
                 elro only has one forest plot

- 250 m for the radius between the bird sites and forest plots

- forest covariates: average of all years with data because of the panel rotation design

# --------------------------------------------------------------------
Workflow:
# Rscript code/format_bird_data/1_ImportData.R
        get netn bird data and extract it
        in:
          *data/src/original/NETN_2020
        out:
          data/out/NETNtib.rds
          data/key_park.rds
# Rscript code/format_veg_data/NETN_forest_data_for_sites.R
        get forest plot level covariates
        in: 
          *data/veg_kateaaron/ForestNETN2024.zip
          *data/tree_sps_harcon.csv
        out:
          data/out/for_plot_covs.rds

#? format_veg_data/get_conhar_baden.r
        get the density of conifer and hardwood trees that are measured by the NETN team to get percentrage of conifer and hardwood per plot

# Rscript code/format_veg_data/get_site_data_rad.R
        find out which forest plots are connected to each bird site according to a 400m and the first closest neighbours
             and weighted mean values for each bird site (weight is the inverse of the distance)
        in:
          data/out/NETNtib.rds
          data/key_park.rds
          *data/out/updated_for_cats.csv
          data/out/for_plot_covs.rds
          data/out/key_bsite.rds
          data/out/key_fsite.rds
          data/out/park_site_UTM.rds
        out:
          data/out/site_covs_fornofor_{radi_dist}m.rds
          data/out/neighbor_fornofor_{radi_dist}m.rds
          data/out/site_covs_hardcon_{radi_dist}m.rds
          data/out/neighbor_hardcon_{radi_dist}m.rds

# Rscript code/format_veg_data/get_park_data.R
        in:
          data/out/for_plot_covs.rds
          data/VAMA_sites.rds
          data/HOFR_sites.rds
          data/ELRO_sites.rds
        out: 
          data/out/park_covs.rds

# Rscript code/format_veg_data/get_coun_data.R
        in:
          data/FIA/

        out:
          data/out/coun_covs.rds

# Rscript code/format_bird_data/2_create_data_files.R
  # Rscript code/format_bird_data/format_data
                filtering visit and field data for only auditory, 50m distance band, and without missing info ('permanetly missing') in any columns we use, e.g. interval number

                in - data/out/NETNtib.rds
        
        create all 'correct' total number of sites, years and occassions, and merge covariate values with bird info

        in - data/out/coun_covs.rds
        in - data/out/park_covs.rds
        in - data/out/site_covs_fornofor_{radi_dist}m.rds or data/out/site_covs_hardcon_{radi_dist}m.rds

        out:
          data/y_dat8.rds 
          data/X.rds
          data/nsite_pk.csv

# Rscript code/fit_model/back2d_covs_scales_2min_spscov.R
        in  - data/y_dat8.rds
        in  - data/X.rds
        in  - data/out/nsite_pk.rds
        in  - data/key_park.rds

        out - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds

# Rscript code/fit_model/run_step1_step2.R

# sbatch: nps_source.sb

# plots.R

# post_hoc


# --------------------------------------------------------------------





## Obsolete/cemetary:
# veg_maps_park.R
            in  - data/veg_maps/
            in  - data/src/key_park.rds
            in  - data/out/park_site.rds
            in  - data/out/bird_site_coords.rds
            in  - data/out/for_sit_coord.rds

------------out - data/out/key_fsite.rds 
------------out - data/out/key_bsite.rds 