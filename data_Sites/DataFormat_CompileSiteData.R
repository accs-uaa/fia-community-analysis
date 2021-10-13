# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Compile Site Data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-10-12
# Usage: Script should be executed in R 4.0.0+.
# Description: "Compile Site Data" merges site data from the AKVEG and FIA databases into a single table.
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

# Define input USDA Plants Database codes table
plants_file = paste(data_folder,
                    'reference',
                    'speciesPlants_03062018.xlsx',
                    sep = '/')

# Define output file
output_site = paste(data_folder,
                    'sites',
                    'sites_all.csv',
                    sep = '/')

# Set AKVEG repository directory
repository = 'C:/Users/timmn/Documents/Repositories/vegetation-plots-database'

# Import required libraries
library(dplyr)
library(readr)
library(readxl)
library(RPostgres)
library(RSQLite)
library(stringr)
library(tibble)
library(tidyr)

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

# Read reference tables into data frame
plants_codes = read_xlsx(plants_file, sheet = 'speciesPlants')

# Define queries for AKVEG
query_adjudicated = 'SELECT * FROM taxon_adjudicated'
query_accepted = 'SELECT * FROM taxon_accepted'

# Read PostgreSQL taxonomy tables into dataframes
taxa_adjudicated = as_tibble(dbGetQuery(akveg_connection, query_adjudicated))
taxa_accepted = as_tibble(dbGetQuery(akveg_connection, query_accepted))

# Create a connection to the FIA SQLite Database
fia_connection = dbConnect(drv = SQLite(), dbname = FIA_database)

# Define plot query for FIA database to select single condition forested plots
query_plot = 'SELECT COND.PLOT as plot_number
    , MAX(COND.CONDID) as max_condition
    , COND.COND_STATUS_CD as status_code
    , PLOT.LAT as latitude
    , PLOT.LON as longitude
FROM COND
    LEFT JOIN PLOT ON COND.PLOT = PLOT.PLOT
GROUP BY plot_number
HAVING max_condition = 1 AND status_code = 1;'
query_vascular = 'SELECT P2VEG_SUBPLOT_SPP.PLOT as plot
    , P2VEG_SUBPLOT_SPP.SUBP as subplot
    , P2VEG_SUBPLOT_SPP.GROWTH_HABIT_CD as strata
    , P2VEG_SUBPLOT_SPP.LAYER as layer
    , P2VEG_SUBPLOT_SPP.COVER_PCT as cover
    , P2VEG_SUBPLOT_SPP.VEG_FLDSPCD as name_original_code
    , PLOT.MEASDAY as day
    , PLOT.MEASMON as month
    , PLOT.MEASYEAR as year
FROM P2VEG_SUBPLOT_SPP
    LEFT JOIN PLOT ON P2VEG_SUBPLOT_SPP.PLOT = PLOT.PLOT
ORDER BY plot;'

# Summarize FIA vascular cover by subplot
vascular_subplot_fia = as_tibble(dbGetQuery(fia_connection, query_vascular)) %>%
  mutate(veg_observe_date = if_else(day < 10,
                                    paste(year, '-0', month, '-0', day, sep =''),
                                    paste(year, '-0', month, '-', day, sep = ''))) %>%
  mutate(site_code = case_when(nchar(as.integer(plot)) == 2 ~ paste('FIAINT_', '000', plot, sep = ''),
                               nchar(as.integer(plot)) == 3 ~ paste('FIAINT_', '00', plot, sep = ''),
                               nchar(as.integer(plot)) == 4 ~ paste('FIAINT_', '0', plot, sep = ''),
                               nchar(as.integer(plot)) == 5 ~ paste('FIAINT_', plot, sep = ''),
                               TRUE ~ 'none')) %>%
  group_by(site_code, veg_observe_date, subplot, name_original_code) %>%
  summarize(max_cover = sum(cover), min_cover = max(cover)) %>%
  mutate(cover = (max_cover + min_cover)/2)

# Summarize FIA cover by plot and add metadata
vascular_plot_fia = vascular_subplot_fia %>%
  group_by(site_code, veg_observe_date, name_original_code) %>%
  summarize(cover = mean(cover)) %>%
  left_join(plants_codes, by = c('name_original_code' = 'codePLANTS')) %>%
  rename(name_original = namePLANTS) %>%
  left_join(taxa_adjudicated, by = c('name_original' = 'name_adjudicated')) %>%
  left_join(taxa_accepted, by = 'accepted_id') %>%
  mutate(project = 'FIA Interior') %>%
  mutate(cover_type = 'total cover')

# Adjudicate accepted species names for FIA data
vascular_plot_fia = vascular_plot_fia %>%
  filter(name_original_code != '2GP' &
           name_original_code != '2GL') %>%
  mutate(name_accepted = case_when(name_original_code == '2FORB' ~ 'Forb',
                                   name_original_code == '2GRAM' ~ 'Graminoid',
                                   name_original_code == '2SHRUB' ~ 'Shrub',
                                   name_original_code == 'DROC' ~ 'Dryas',
                                   name_original_code == 'LYOB' ~ 'Spore-bearing',
                                   name_original_code == 'VAOX' ~ 'Oxycoccus microcarpus',
                                   TRUE ~ name_accepted)) %>%
  select(project, site_code, veg_observe_date, cover_type, name_accepted, cover)

# Find distinct plots with at least 5% foliar cover of summed tree species
forest_fia = vascular_plot_fia %>%
  filter(name_accepted == 'Picea sitchensis' |
           name_accepted == 'Picea ×lutzii' |
           name_accepted == 'Picea glauca' |
           name_accepted == 'Picea mariana' |
           name_accepted == 'Larix laricina' |
           name_accepted == 'Betula kenaica' |
           name_accepted == 'Betula neoalaskana' |
           name_accepted == 'Populus tremuloides' |
           name_accepted == 'Populus trichocarpa' |
           name_accepted == 'Populus balsamifera') %>%
  group_by(site_code) %>%
  summarize(tree_cover = sum(cover)) %>%
  filter(tree_cover >= 5) %>%
  select(site_code) %>%
  distinct()

# Define plot metadata
site_fia = as_tibble(dbGetQuery(fia_connection, query_plot)) %>%
  mutate(site_code = case_when(nchar(as.integer(plot_number)) == 2 ~
                                 paste('FIAINT_', '000', plot_number, sep = ''),
                               nchar(as.integer(plot_number)) == 3 ~
                                 paste('FIAINT_', '00', plot_number, sep = ''),
                               nchar(as.integer(plot_number)) == 4 ~
                                 paste('FIAINT_', '0', plot_number, sep = ''),
                               nchar(as.integer(plot_number)) == 5 ~
                                 paste('FIAINT_', plot_number, sep = ''),
                               TRUE ~ 'none')) %>%
  mutate(project = 'FIA Interior') %>%
  mutate(perspective = 'ground') %>%
  mutate(cover_method = 'semi-quantitative visual estimate') %>%
  mutate(scope_vascular = 'partial') %>%
  mutate(scope_bryophyte = 'functional group or life form') %>%
  mutate(scope_lichen = 'functional group or life form') %>%
  mutate(plot_dimensions = '804 radius') %>%
  mutate(datum = 'NAD83') %>%
  mutate(error = 804) %>%
  inner_join(forest_fia, by = 'site_code') %>%
  select(site_code, project, perspective, cover_method, scope_vascular, scope_bryophyte, scope_lichen, plot_dimensions, datum, latitude, longitude, error)

#### FORMAT AKVEG DATA
####------------------------------

# Define queries for AKVEG
query_site = 'SELECT site.site_code as site_code
     , project.project_abbr as project
     , perspective.perspective as perspective
     , cover_method.cover_method as cover_method
     , scope_vascular.scope as scope_vascular
     , scope_bryophyte.scope as scope_bryophyte
     , scope_lichen.scope as scope_lichen
     , plot_dimensions.plot_dimensions as plot_dimensions
     , datum.datum as datum
     , site.latitude as latitude
     , site.longitude as longitude
     , site.error as error
FROM site
    LEFT JOIN project ON site.project_id = project.project_id
    LEFT JOIN perspective ON site.perspective_id = perspective.perspective_id
    LEFT JOIN cover_method ON site.cover_method_id = cover_method.cover_method_id
    LEFT JOIN scope scope_vascular ON site.scope_vascular_id = scope_vascular.scope_id
    LEFT JOIN scope scope_bryophyte ON site.scope_bryophyte_id = scope_bryophyte.scope_id
    LEFT JOIN scope scope_lichen ON site.scope_lichen_id = scope_lichen.scope_id
    LEFT JOIN plot_dimensions ON site.plot_dimensions_id = plot_dimensions.plot_dimensions_id
    LEFT JOIN datum ON site.datum_id = datum.datum_id
ORDER BY site_id;'
query_cover = 'SELECT cover.cover_id as cover_id
     , project.project_abbr as project
     , site.site_code as site_code
     , cover.veg_observe_date as veg_observe_date
     , veg_observer.personnel as veg_observer
     , veg_recorder.personnel as veg_recorder
     , cover_type.cover_type as cover_type
     , taxon_accepted.name_accepted as name_accepted
     , cover.cover as cover
FROM cover
    LEFT JOIN project ON cover.project_id = project.project_id
    LEFT JOIN site ON cover.site_id = site.site_id
    LEFT JOIN personnel veg_observer ON cover.veg_observer_id = veg_observer.personnel_id
    LEFT JOIN personnel veg_recorder ON cover.veg_recorder_id = veg_recorder.personnel_id
    LEFT JOIN cover_type ON cover.cover_type_id = cover_type.cover_type_id
    LEFT JOIN taxon_adjudicated ON cover.adjudicated_id = taxon_adjudicated.adjudicated_id
    LEFT JOIN taxon_accepted ON taxon_adjudicated.accepted_id = taxon_accepted.accepted_id
ORDER BY cover_id;'

# Read and format PostgreSQL data tables into dataframes
site_akveg = as_tibble(dbGetQuery(akveg_connection, query_site))
cover_akveg = as_tibble(dbGetQuery(akveg_connection, query_cover))

# Find distinct plots with at least 5% foliar cover of summed tree species
forest_akveg = cover_akveg %>%
  filter(name_accepted == 'Picea sitchensis' |
           name_accepted == 'Picea ×lutzii' |
           name_accepted == 'Picea glauca' |
           name_accepted == 'Picea mariana' |
           name_accepted == 'Larix laricina' |
           name_accepted == 'Betula kenaica' |
           name_accepted == 'Betula neoalaskana' |
           name_accepted == 'Populus tremuloides' |
           name_accepted == 'Populus trichocarpa' |
           name_accepted == 'Populus balsamifera') %>%
  group_by(site_code) %>%
  summarize(tree_cover = sum(cover)) %>%
  filter(tree_cover >= 5) %>%
  select(site_code) %>%
  distinct()

# Subset sites from AKVEG to those with trees that were observed from the ground
site_selected = site_akveg %>%
  filter(perspective == 'ground') %>%
  inner_join(forest_akveg, by = 'site_code') %>%
  filter(plot_dimensions != 'unknown')

#### COMBINE AND EXPORT DATA
####------------------------------

# Bind rows from AKVEG and FIA databases
site_data = bind_rows(site_selected, site_fia)

# Export data
write.csv(site_data, file = output_site, fileEncoding = 'UTF-8', row.names = FALSE)