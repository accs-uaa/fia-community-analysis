# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Compile Site Data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-04-30
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

# Find distinct plots with at least 3% foliar cover of at least one tree species
forest_sites = cover_akveg %>%
  filter((name_accepted == 'Picea sitchensis' |
            name_accepted == 'Picea ×lutzii' |
            name_accepted == 'Picea glauca' |
            name_accepted == 'Picea mariana' |
            name_accepted == 'Larix laricina' |
            name_accepted == 'Betula kenaica' |
            name_accepted == 'Betula neoalaskana' |
            name_accepted == 'Populus tremuloides' |
            name_accepted == 'Populus trichocarpa' |
            name_accepted == 'Populus balsamifera') &
           cover >= 3) %>%
  select(site_code) %>%
  distinct()

# Subset sites from AKVEG to those with trees that were observed from the ground
site_selected = site_akveg %>%
  filter(perspective == 'ground') %>%
  inner_join(forest_sites, by = 'site_code')

# Create a connection to the FIA SQLite Database
fia_connection = dbConnect(drv = SQLite(), dbname = FIA_database)

# Define plot query for FIA database
query_plot = 'SELECT PLOT.PLOT as plot
    , PLOT.LAT as latitude
    , PLOT.LON as longitude
FROM PLOT;'

# Read plot data into tibble
plot_table = as_tibble(dbGetQuery(fia_connection, query_plot))

# Define plot metadata
site_fia = plot_table %>%
  mutate(site_code = case_when(nchar(as.integer(plot)) == 2 ~ paste('FIAINT_', '000', plot, sep = ''),
                               nchar(as.integer(plot)) == 3 ~ paste('FIAINT_', '00', plot, sep = ''),
                               nchar(as.integer(plot)) == 4 ~ paste('FIAINT_', '0', plot, sep = ''),
                               nchar(as.integer(plot)) == 5 ~ paste('FIAINT_', plot, sep = ''),
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
  select(site_code, project, perspective, cover_method, scope_vascular, scope_bryophyte, scope_lichen, plot_dimensions, datum, latitude, longitude, error)

# Bind rows from AKVEG and FIA databases
site_data = bind_rows(site_selected, site_fia)

# Export data
write.csv(site_data, file = output_site, fileEncoding = 'UTF-8', row.names = FALSE)