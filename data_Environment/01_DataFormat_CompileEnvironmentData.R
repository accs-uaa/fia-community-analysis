# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Compile observed environment data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-10-06
# Usage: Script should be executed in R 4.0.0+.
# Description: "Compile observed environment data" merges and reconciles the observed environmental characteristics from the FIA Interior database and the AKVEG database.
# ---------------------------------------------------------------------------

# Set root directory
drive = 'N:'
root_folder = 'ACCS_Work'

# Define input folders
data_folder = paste(drive,
                    root_folder,
                    'Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input',
                    sep = '/')

# Define input FIA database
FIA_database = paste(data_folder,
                     'FIA',
                     'SQLite_FIADB_INTAK_TANANA.db',
                     sep = '/')

# Define input clipped sites file
sites_file = paste(data_folder,
                   'sites',
                   'sites_nab.xlsx',
                   sep = '/')

# Define output file
output_file = paste(data_folder,
                    'environment',
                    'environment_nab.csv',
                    sep = '/')

# Set repository directory
repository = 'C:/Users/timmn/Documents/Repositories/vegetation-plots-database'

# Import required libraries
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(RPostgres)
library(RSQLite)
library(stringr)
library(tibble)
library(tidyr)

# Read tables into data frames
site_data = read_excel(sites_file, sheet = 'sites_nab')

# Import database connection function
connection_script = paste(repository,
                          'package_DataProcessing',
                          'connectDatabasePostGreSQL.R',
                          sep = '/')
source(connection_script)

# Create a connection to the AKVEG PostgreSQL database
authentication = paste(drive,
                       root_folder,
                       'Administrative/Credentials/accs-postgresql/authentication_akveg.csv',
                       sep = '/')
akveg_connection = connect_database_postgresql(authentication)

#### FORMAT AND RECONCILE FIA DATA
####------------------------------

# Create a connection to the FIA SQLite Database
fia_connection = dbConnect(drv = SQLite(), dbname = FIA_database)

# Define query for FIA database
query_plot_fia = 'SELECT PLOT.PLOT as plot
    , PLOT.MEASDAY as day
    , PLOT.MEASMON as month
    , PLOT.MEASYEAR as year
    , PLOT.TOPO_POSITION_PNW as topographic_position
    , PLOT.ELEV as elevation_fuzzed
FROM PLOT
ORDER BY plot;'
query_subplot_fia = 'SELECT SUBPLOT.PLOT as plot
    , SUBPLOT.SLOPE as slope_percent
    , SUBPLOT.ASPECT as aspect
FROM SUBPLOT
ORDER BY plot'
query_cond_fia = 'SELECT COND.PLOT as plot
    , COND.CONDID as condition
    , COND.PHYSCLCD as physiographic_class
FROM COND
ORDER BY plot'
query_soils_fia = 'SELECT SOILS_SAMPLE_LOC_PNWRS.PLOT as plot
    , SOILS_SAMPLE_LOC_PNWRS.SUBP as subplot
    , SOILS_SAMPLE_LOC_PNWRS.UNFROZEN_DEPTH_N as frost_depth_n
    , SOILS_SAMPLE_LOC_PNWRS.UNFROZEN_DEPTH_E as frost_depth_e
    , SOILS_SAMPLE_LOC_PNWRS.UNFROZEN_DEPTH_S as frost_depth_s
    , SOILS_SAMPLE_LOC_PNWRS.UNFROZEN_DEPTH_W as frost_depth_w
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_MOSS_THICKNESS_N as litter_moss_n
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_MOSS_THICKNESS_E as litter_moss_e
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_MOSS_THICKNESS_S as litter_moss_s
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_MOSS_THICKNESS_W as litter_moss_w
    , SOILS_SAMPLE_LOC_PNWRS.DEAD_MOSS_THICKNESS as dead_moss_depth
    , SOILS_SAMPLE_LOC_PNWRS.GREEN_MOSS_LICHEN_THICKNESS as moss_depth
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_THICKNESS_N as litter_n
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_THICKNESS_E as litter_e
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_THICKNESS_S as litter_s
    , SOILS_SAMPLE_LOC_PNWRS.LITTER_THICKNESS_W as litter_w
    , SOILS_SAMPLE_LOC_PNWRS.LOWER_HUM_DUFF_THICKNESS as duff_thickness
    , SOILS_SAMPLE_LOC_PNWRS.IDENTIFIABLE_ORG_THICKNESS as duff_depth
FROM SOILS_SAMPLE_LOC_PNWRS
ORDER BY plot'
query_ph_fia = 'SELECT SOILS_LAB_PNWRS.PLOT as plot
    , SOILS_LAB_PNWRS.SOILS_SUBP as subplot
    , SOILS_LAB_PNWRS.PH_H2O as soil_ph
FROM SOILS_LAB_PNWRS
ORDER BY plot'

# Summarize subplot data to plot
subplot_fia = as_tibble(dbGetQuery(fia_connection, query_subplot_fia)) %>%
    drop_na() %>%
    mutate(slope = (atan2(slope_percent, 100))*180 / pi) %>%
    group_by(plot) %>%
    summarize(slope_obs = mean(slope),
              aspect_obs = mean(aspect))

# Summarize condition data to plot
condition_fia = as_tibble(dbGetQuery(fia_connection, query_cond_fia)) %>%
    drop_na() %>%
    mutate(moisture_numeric = case_when(physiographic_class == 11 ~ 10,
                                       physiographic_class == 12 ~ 10,
                                       physiographic_class == 13 ~ 10,
                                       physiographic_class == 19 ~ 10,
                                       physiographic_class == 21 ~ 20,
                                       physiographic_class == 22 ~ 20,
                                       physiographic_class == 23 ~ 20,
                                       physiographic_class == 24 ~ 20,
                                       physiographic_class == 25 ~ 20,
                                       physiographic_class == 29 ~ 20,
                                       physiographic_class == 31 ~ 30,
                                       physiographic_class == 32 ~ 30,
                                       physiographic_class == 33 ~ 30,
                                       physiographic_class == 34 ~ 30,
                                       physiographic_class == 35 ~ 30,
                                       physiographic_class == 39 ~ 30,
                                       TRUE ~ -999)) %>%
    filter(moisture_numeric != -999) %>%
    group_by(plot) %>%
    summarize(moisture_numeric = mean(moisture_numeric)) %>%
    mutate(moisture_simple = case_when(moisture_numeric < 15 ~ 'xeric',
                                       moisture_numeric >= 15 & moisture_numeric < 25 ~ 'mesic',
                                       moisture_numeric >= 25 ~ 'hydric',
                                       TRUE ~ '-999')) %>%
    select(plot, moisture_simple)

# Summarize soils data to plot
ph_fia = as_tibble(dbGetQuery(fia_connection, query_ph_fia)) %>%
    drop_na() %>%
    group_by(plot) %>%
    summarize(soil_ph_10 = mean(soil_ph))

# Summarize soils data to plot
soils_fia = as_tibble(dbGetQuery(fia_connection, query_soils_fia)) %>%
    mutate(depth_restrictive_layer = (frost_depth_n + frost_depth_e + frost_depth_s + frost_depth_w)/4) %>%
    mutate(litter_moss_depth = (litter_moss_n + litter_moss_e + litter_moss_s + litter_moss_w)/4) %>%
    mutate(litter_depth = (litter_n + litter_e + litter_s + litter_w)/4) %>%
    mutate(duff_depth = if_else(is.na(duff_depth), duff_thickness, duff_depth, missing = NULL)) %>%
    mutate(litter_moss_depth = replace_na(litter_moss_depth, 0)) %>%
    mutate(litter_depth = replace_na(litter_depth, 0)) %>%
    mutate(moss_depth = replace_na(moss_depth, 0)) %>%
    mutate(dead_moss_depth = replace_na(dead_moss_depth, 0)) %>%
    mutate(duff_depth = replace_na(duff_depth, 0)) %>%
    mutate(depth_moss_duff = litter_moss_depth +
               litter_depth +
               moss_depth +
               dead_moss_depth +
               duff_depth) %>%
    group_by(plot) %>%
    summarize(depth_moss_duff = mean(depth_moss_duff),
              depth_restrictive_layer = mean(depth_restrictive_layer)) %>%
    drop_na() %>%
    mutate(restrictive_type = 'frost')

# Summarize plot data
env_fia = as_tibble(dbGetQuery(fia_connection, query_plot_fia)) %>%
    mutate(elevation_fuzzed_m = elevation_fuzzed * 0.3048) %>%
    mutate(slope_shape = case_when(topographic_position == 1 ~ 'flat',
                                   topographic_position == 2 ~ 'convex',
                                   topographic_position == 3 ~ 'convex',
                                   topographic_position == 4 ~ 'no rounding',
                                   topographic_position == 5 ~ 'concave',
                                   topographic_position == 6 ~ 'concave',
                                   topographic_position == 7 ~ 'flat',
                                   topographic_position == 8 ~ 'flat',
                                   topographic_position == 9 ~ 'flat',
                                   TRUE ~ '-999')) %>%
    mutate(env_observe_date = make_date(year, month, day)) %>%
    mutate(day = yday(env_observe_date)) %>%
    mutate(site_code = case_when(nchar(as.integer(plot)) == 2 ~
                                     paste('FIAINT_', '000', plot, sep = ''),
                                 nchar(as.integer(plot)) == 3 ~
                                     paste('FIAINT_', '00', plot, sep = ''),
                                 nchar(as.integer(plot)) == 4 ~
                                     paste('FIAINT_', '0', plot, sep = ''),
                                 nchar(as.integer(plot)) == 5 ~
                                     paste('FIAINT_', plot, sep = ''),
                                 TRUE ~ 'none')) %>%
    inner_join(site_data, by = 'site_code') %>%
    left_join(subplot_fia, by = 'plot') %>%
    left_join(condition_fia, by = 'plot') %>%
    left_join(ph_fia, by = 'plot') %>%
    left_join(soils_fia, by = 'plot') %>%
    mutate(homogenous = 1) %>%
    mutate(depth_moss_duff = replace_na(depth_moss_duff, -999)) %>%
    mutate(depth_restrictive_layer = replace_na(depth_restrictive_layer, -999)) %>%
    mutate(restrictive_type = replace_na(restrictive_type, -999)) %>%
    mutate(soil_ph_10 = replace_na(soil_ph_10, -999)) %>%
    mutate(soil_ph_30 = -999) %>%
    mutate(water_ph = -999) %>%
    select(site_code, project, year, day, latitude, longitude, datum, plot_dimensions, slope_shape,
           moisture_simple, depth_moss_duff, depth_restrictive_layer, restrictive_type, soil_ph_10,
           soil_ph_30, water_ph, homogenous)

#### FORMAT AND RECONCILE AKVEG DATA
####------------------------------

query_env_akveg = 'SELECT environment.environment_id as environment_id
     , project.project_abbr as project
     , site.site_code as site_code
     , environment.env_observe_date as env_observe_date
     , macrotopography.macrotopography as macrotopography
     , moisture.moisture as moisture
     , environment.depth_moss_duff as depth_moss_duff
     , environment.depth_restrictive_layer as depth_restrictive_layer
     , restrictive_type.restrictive_type as restrictive_type
     , environment.soil_ph_10 as soil_ph_10
     , environment.soil_ph_30 as soil_ph_30
     , environment.water_ph as water_ph
     , environment.homogenous as homogenous
FROM environment
    LEFT JOIN project ON environment.project_id = project.project_id
    LEFT JOIN site ON environment.site_id = site.site_id
    LEFT JOIN stratification ON environment.strata_id = stratification.strata_id
    LEFT JOIN physiography ON environment.physiography_id = physiography.physiography_id
    LEFT JOIN geomorphology ON environment.geomorphology_id = geomorphology.geomorphology_id
    LEFT JOIN macrotopography ON environment.macrotopography_id = macrotopography.macrotopography_id
    LEFT JOIN microtopography ON environment.microtopography_id = microtopography.microtopography_id
    LEFT JOIN drainage ON environment.drainage_id = drainage.drainage_id
    LEFT JOIN moisture ON environment.moisture_id = moisture.moisture_id
    LEFT JOIN soil_class ON environment.soil_class_id = soil_class.soil_class_id
    LEFT JOIN restrictive_type ON environment.restrictive_type_id = restrictive_type.restrictive_type_id
    LEFT JOIN disturbance ON environment.disturbance_id = disturbance.disturbance_id
ORDER BY environment_id;'

# Reconcile AKVEG data to FIA
env_akveg = as_tibble(dbGetQuery(akveg_connection, query_env_akveg)) %>%
    mutate(env_observe_date = as.character(env_observe_date)) %>%
    inner_join(site_data, by = 'site_code') %>%
    mutate(slope_shape = case_when(macrotopography == 'alluvial cone' ~ 'concave',
                                   macrotopography == 'alluvial fan' ~ 'concave',
                                   macrotopography == 'alluvial flat' ~ 'flat',
                                   macrotopography == 'beach plain' ~ 'flat',
                                   macrotopography == 'beach terrace' ~ 'flat',
                                   macrotopography == 'bench' ~ 'flat',
                                   macrotopography == 'cirque' ~ 'concave',
                                   macrotopography == 'colluvial apron' ~ 'convex',
                                   macrotopography == 'depression' ~ 'concave',
                                   macrotopography == 'draw' ~ 'concave',
                                   macrotopography == 'dunes' ~ 'convex',
                                   macrotopography == 'esker' ~ 'convex',
                                   macrotopography == 'floodplain abandoned' ~ 'flat',
                                   macrotopography == 'floodplain basin' ~ 'concave',
                                   macrotopography == 'floodplain terrace' ~ 'flat',
                                   macrotopography == 'non-patterned drained thaw lake basin' ~ 'concave',
                                   macrotopography == 'pingo' ~ 'convex',
                                   macrotopography == 'plane' ~ 'flat',
                                   macrotopography == 'polygons flat-center' ~ 'flat',
                                   macrotopography == 'polygons high-center' ~ 'flat',
                                   macrotopography == 'polygons low-center' ~ 'flat',
                                   macrotopography == 'polygons low-center coalescent' ~ 'flat',
                                   macrotopography == 'polygons mixed' ~ 'flat',
                                   macrotopography == 'saddle' ~ 'concave',
                                   macrotopography == 'slope concave' ~ 'concave',
                                   macrotopography == 'slope convex' ~ 'convex',
                                   macrotopography == 'slope planar' ~ 'no rounding',
                                   macrotopography == 'terrace' ~ 'flat',
                                   macrotopography == 'tidal flat' ~ 'flat',
                                   macrotopography == 'concave' ~ 'concave',
                                   macrotopography == 'convex' ~ 'convex',
                                   macrotopography == 'hummocks' ~ 'flat',
                                   macrotopography == 'gelifluction lobes' ~ 'convex',
                                   macrotopography == 'ice-cored mounds' ~ 'convex',
                                   macrotopography == 'mounds' ~ 'convex',
                                   macrotopography == 'mounds caused by trees' ~ 'convex',
                                   macrotopography == 'mounds caused by wildlife' ~ 'convex',
                                   macrotopography == 'peat mounds' ~ 'convex',
                                   macrotopography == 'plane' ~ 'flat',
                                   macrotopography == 'polygonal' ~ 'flat',
                                   macrotopography == 'solifluction lobes' ~ 'convex',
                                   macrotopography == 'tussocks' ~ 'flat',
                                   TRUE ~ '-999')) %>%
    mutate(moisture_simple = case_when(moisture == 'marine' ~ 'aquatic',
                                       moisture == 'brackish' ~ 'aquatic',
                                       moisture == 'aquatic' ~ 'aquatic',
                                       moisture == 'hydric-aquatic' ~ 'aquatic',
                                       moisture == 'hydric' ~ 'hydric',
                                       moisture == 'hygric-hydric' ~ 'hydric',
                                       moisture == 'hygric' ~ 'hydric',
                                       moisture == 'mesic-hygric' ~ 'mesic',
                                       moisture == 'mesic' ~ 'mesic',
                                       moisture == 'xeric-mesic' ~ 'mesic',
                                       moisture == 'xeric' ~ 'xeric',
                                       TRUE ~ '-999')) %>%
    mutate(env_observe_date = ymd(env_observe_date)) %>%
    mutate(year = year(env_observe_date)) %>%
    mutate(day = yday(env_observe_date)) %>%
    mutate(depth_moss_duff = replace_na(depth_moss_duff, -999)) %>%
    mutate(depth_restrictive_layer = replace_na(depth_restrictive_layer, -999)) %>%
    mutate(restrictive_type = replace_na(restrictive_type, '-999')) %>%
    mutate(soil_ph_10 = replace_na(soil_ph_10, -999)) %>%
    mutate(soil_ph_30 = replace_na(soil_ph_30, -999)) %>%
    mutate(water_ph = replace_na(water_ph, -999)) %>%
    rename(project = project.x) %>%
    select(site_code, project, year, day, latitude, longitude, datum, plot_dimensions, slope_shape,
           moisture_simple, depth_moss_duff, depth_restrictive_layer, restrictive_type, soil_ph_10,
           soil_ph_30, water_ph, homogenous)

#### COMBINE AND EXPORT DATA
####------------------------------

# Combine FIA and AKVEG data
env_all = rbind(env_fia, env_akveg)

# Export data
write.csv(env_all, file = output_file, fileEncoding = 'UTF-8', row.names = FALSE)