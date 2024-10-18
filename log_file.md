# ---------------------- my repository log file ---------------------

DOOOOO(ing):

- 

- 

# --------------------------------------------------------------------
TO DO:

- plotar vizinhos com o cover map

( ) Kate: if genus is kept in the diversity calculations for the FIA data
( ) Kate: are these the same? siteBA_large_s, parkBA_large_s, counPER_late_s


# --------------------------------------------------------------------
Assumptions/decisions:

- add covariate value inputation - some site level have no forest covariates; right now im adding zero

- parks removed: acadia is too big
                 sair is too different (open areas)
                 elro only has one forest plot

- 500 m for the radius between the sites make sense for bird home range, but is that meaningful? now using 500m

- year of environmental covariates: 2020

# --------------------------------------------------------------------
Workflow:

# format_veg_data/get_site_data.R
            in  - data/out/NETNtib.rds
            in  - data/src/key_park.rds
            in  - data/veg_kateaaron/NETN_forest_data_2006-2023.rds
            in  - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds
            in  - data/veg_kateaaron/for_sites.rds

            out - data/out/for_sit2.rds
            out - data/out/site_covs_[xx]m.rds
            out - data/out/park_site.rds
            out - data/out/for_sit_coord.rds
            out - data/out/bird_site_coords.rds
            out - data/out/close_points_f.rds

# format_veg_data/get_park_data.R
            in  - data/veg_kateaaron/NETN_forest_data_2006-2023.rds

            out - data/out/park_covs.rds

# format_veg_data/local_diversity.R

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

# format_bird_data/back2d_covs_scales.R
            in  - data/y_dat8.rds
            in  - data/X.rds
in  - data/out/nsite_pk.rds
in  - data/src/key_park.rds

            out - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds
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