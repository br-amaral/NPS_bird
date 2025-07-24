# ---------------------- my repository log file ---------------------

DOOOOO(ing):



# --------------------------------------------------------------------
TO DO:




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
# code/format_bird_data/1_ImportData.R
        get netn bird data and extract it
        in:
          *data/src/original/NETN_2020
        out:
          data/out/NETNtib.rds
          data/key_park.rds
# code/format_veg_data/NETN_forest_data_for_sites.R
        get forest plot level covariates
        in: 
          *data/veg_kateaaron/ForestNETN2024.zip
          *data/tree_sps_harcon.csv
        out:
          data/out/for_plot_covs.rds

#? format_veg_data/get_conhar_baden.r
        get the density of conifer and hardwood trees that are measured by the NETN team to get percentrage of conifer and hardwood per plot

# format_veg_data/get_site_data_rad.R
        find out which forest plots are connected to each bird site according to a 400m and the first closest neighbours
             and weighted mean values for each bird site (weight is the inverse of the distance)
        in:
          data/out/NETNtib.rds
          data/key_park.rds
          *data/out/updated_for_cats.csv
          data/out/for_plot_covs.rds


          
#?        out - data/out/for_sit2.rds
#?        out - data/out/site_covs_[xx]m.rds
#?        out - data/out/park_site.rds
#?        out - data/out/for_sit_coord.rds
#?        out - data/out/bird_site_coords.rds
#?        out - data/out/close_points_f.rds

# format_veg_data/get_park_data.R
#?        in  - data/veg_kateaaron/NETN_forest_data_2006-2023.rds

#?        out - data/out/park_covs.rds


# format_veg_data/FIA_getdata.R

# format_veg_data/get_coun_data.R
        in  - 'data/FIA/'

        out - data/out/coun_covs.rds

# format_bird_data/2_create_data_files.R
        in  - data/out/close_points_fcovs.rds
        in  - data/NETN-forest/tree_ba_tab_park.rds
        in  - data/NETN-forest/tree_den_tab_park.rds
        in  - data/NETN-forest/stand_struc_tab_park.rds
        in  - data/FIA/out/bas_area_tot_import.rds
        in  - data/FIA/out/tree_acre_tot_import.rds
        in  - data/FIA/out/stand_struc_import.rds
        in  - data/park_raster/{pk[i]}_pb.rds
        in  - y1
        in  - visits (data/out/visits.rds)
        in  - yr_pk

        out - data/src/sites_park_tib.rds
        out - data/out/site_n_key.rds
        out - data/out/y_dat3.rds
        out - data/y_dat8.rds
        out - data/X.rds 
        out - data/sps_pk_nth.rds

# format_bird_data/back2d_covs_scales_2min_spscov.R
        in  - data/y_dat8.rds
        in  - data/X.rds
        in  - data/out/nsite_pk.rds
        in  - data/src/key_park.rds

        out - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds
TODO: for now, im putting zeros in the occasions that have no environmental data (mean)

# multiple_single_sps_spscovs.R

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