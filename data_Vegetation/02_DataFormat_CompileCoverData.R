# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Compile Cover Data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-05-01
# Usage: Script should be executed in R 4.0.0+.
# Description: "Compile Cover Data" merges cover values from multiple layers from the FIA database, finds plot level means for FIA data, and reconciles nomenclature of FIA data to the accepted names of the AKVEG taxonomic standard. The script then combines the formatted and reconciled FIA data with data from the AKVEG database.
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

# Define input site table
sites_file = paste(data_folder,
                  'sites',
                  'sites_all.csv',
                  sep = '/')

# Define input USDA Plants Database codes table
plants_file = paste(data_folder,
                    'reference',
                    'speciesPlants_03062018.xlsx',
                    sep = '/')

# Define nonvascular functional groups table
groups_file = paste(data_folder,
                    'reference',
                    'nonvascular_functional_groups.csv',
                    sep = '/')

# Define analysis codes table
codes_file = paste(data_folder,
                   'reference',
                   'codes_analysis.xlsx',
                   sep = '/')

# Define output file
output_cover = paste(data_folder,
                     'vegetation',
                     'cover_all.csv',
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

# Read reference tables into data frame
plants_codes = read_excel(plants_file, sheet = 'speciesPlants')
functional_groups = read.csv(groups_file, encoding = 'UTF-8')
sites_all = read.csv(sites_file, encoding = 'UTF-8')
codes_analysis = read_excel(codes_file, sheet = 'codes')

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

# Define queries for AKVEG
query_adjudicated = 'SELECT * FROM taxon_adjudicated'
query_accepted = 'SELECT * FROM taxon_accepted'

# Read PostgreSQL taxonomy tables into dataframes
taxa_adjudicated = as_tibble(dbGetQuery(akveg_connection, query_adjudicated))
taxa_accepted = as_tibble(dbGetQuery(akveg_connection, query_accepted))

# Create a connection to the FIA SQLite Database
fia_connection = dbConnect(drv = SQLite(), dbname = FIA_database)

# Define query for FIA database
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
query_nonvascular = 'SELECT GRND_LYR_FNCTL_GRP.PLOT as plot
    , GRND_LYR_FNCTL_GRP.SUBP as subplot
    , GRND_LYR_FNCTL_GRP.TRANSECT as transect
    , GRND_LYR_FNCTL_GRP.COVER_CLASS_CD as cover
    , GRND_LYR_FNCTL_GRP.FUNCTIONAL_GROUP_CD as name_original_code
    , PLOT.MEASDAY as day
    , PLOT.MEASMON as month
    , PLOT.MEASYEAR as year
FROM GRND_LYR_FNCTL_GRP
    LEFT JOIN PLOT ON GRND_LYR_FNCTL_GRP.PLOT = PLOT.PLOT
ORDER BY plot;'

# Read FIA cover data into tibbles
vascular_fia = as_tibble(dbGetQuery(fia_connection, query_vascular))
nonvascular_fia = as_tibble(dbGetQuery(fia_connection, query_nonvascular))

# Summarize FIA vascular cover by subplot
vascular_subplot_fia = vascular_fia %>%
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

# Identify FIA functional groups
nonvascular_subplot_fia = nonvascular_fia %>%
  filter(name_original_code != 'N' |
           name_original_code != 'Y') %>%
  mutate(cover = if_else(cover == 'T', '0', cover)) %>%
  mutate(cover = as.numeric(cover)) %>%
  mutate(name_accepted = case_when(name_original_code == 'CC' ~ 'Biotic Crust',
                                   name_original_code == 'CO' ~ 'Orange Lichen',
                                   name_original_code == 'LF' ~ 'Forage Lichen',
                                   name_original_code == 'LL' ~ 'Foliose Lichen',
                                   name_original_code == 'LLFOL' ~ 'Foliose Lichen',
                                   name_original_code == 'LLFRU' ~ 'Fruticose Lichen',
                                   name_original_code == 'LN' ~ 'N-fixing Foliose Lichen',
                                   name_original_code == 'LNFOL' ~ 'N-fixing Foliose Lichen',
                                   name_original_code == 'LNFRU' ~ 'N-fixing Fruticose Lichen',
                                   name_original_code == 'LR' ~ 'Fruticose Lichen',
                                   name_original_code == 'LU' ~ 'N-fixing Fruticose Lichen',
                                   name_original_code == 'MF' ~ 'Feathermoss',
                                   name_original_code == 'MN' ~ 'N-fixing Feathermoss',
                                   name_original_code == 'MS' ~ 'Sphagnum',
                                   name_original_code == 'MT' ~ 'Turf Moss',
                                   name_original_code == 'VF' ~ 'Thalloid Liverwort',
                                   name_original_code == 'VS' ~ 'Liverwort',
                                   TRUE ~ 'Other'))

# Summarize FIA nonvascular cover by plot
nonvascular_plot_fia = nonvascular_subplot_fia %>%
  mutate(veg_observe_date = if_else(day < 10,
                                    paste(year, '-0', month, '-0', day, sep =''),
                                    paste(year, '-0', month, '-', day, sep = ''))) %>%
  mutate(site_code = case_when(nchar(as.integer(plot)) == 2 ~ paste('FIAINT_', '000', plot, sep = ''),
                               nchar(as.integer(plot)) == 3 ~ paste('FIAINT_', '00', plot, sep = ''),
                               nchar(as.integer(plot)) == 4 ~ paste('FIAINT_', '0', plot, sep = ''),
                               nchar(as.integer(plot)) == 5 ~ paste('FIAINT_', plot, sep = ''),
                               TRUE ~ 'none')) %>%
  group_by(site_code, veg_observe_date, name_accepted) %>%
  summarize(cover = mean(cover)) %>%
  mutate(project = 'FIA Interior') %>%
  mutate(cover_type = 'total cover') %>%
  select(project, site_code, veg_observe_date, cover_type, name_accepted, cover)

# Combine vascular and nonvascular FIA cover data
cover_fia = rbind(vascular_plot_fia, nonvascular_plot_fia)

# Select single condition forested plots from FIA
sites_fia = sites_all %>%
  filter(project == 'FIA Interior') %>%
  select(site_code)

# Limit cover data to single condition forested plots
cover_fia = cover_fia %>%
  inner_join(sites_fia, by = 'site_code') %>%
  select(project, site_code, veg_observe_date, cover_type, name_accepted, cover)

#### FORMAT AKVEG DATA
####------------------------------

# Define query for AKVEG
query_cover = 'SELECT cover.cover_id as cover_id
     , project.project_abbr as project
     , site.site_code as site_code
     , cover.veg_observe_date as veg_observe_date
     , cover_type.cover_type as cover_type
     , taxon_accepted.name_accepted as name_accepted
     , category.category as category
     , cover.cover as cover
FROM cover
    LEFT JOIN project ON cover.project_id = project.project_id
    LEFT JOIN site ON cover.site_id = site.site_id
    LEFT JOIN cover_type ON cover.cover_type_id = cover_type.cover_type_id
    LEFT JOIN taxon_adjudicated ON cover.adjudicated_id = taxon_adjudicated.adjudicated_id
    LEFT JOIN taxon_accepted ON taxon_adjudicated.accepted_id = taxon_accepted.accepted_id
    LEFT JOIN hierarchy ON taxon_accepted.hierarchy_id = hierarchy.hierarchy_id
    LEFT JOIN category ON hierarchy.category_id = category.category_id
WHERE cover.cover_type_id = 2
ORDER BY cover_id;'

# Select forested plots from AKVEG
sites_akveg = sites_all %>%
  filter(project != 'FIA Interior') %>%
  select(site_code)

# Read PostgreSQL data into dataframe
cover_akveg = as_tibble(dbGetQuery(akveg_connection, query_cover))
cover_akveg = cover_akveg %>%
  mutate(veg_observe_date = as.character(veg_observe_date)) %>%
  inner_join(sites_akveg, by = 'site_code')

# Split vascular and nonvascular data for AKVEG
vascular_akveg = cover_akveg %>%
  filter(category == 'Eudicot' |
           category == 'Fern' |
           category == 'Gymnosperm' |
           category == 'Horsetail' |
           category == 'Lycophyte' |
           category == 'Monocot')
nonvascular_akveg = cover_akveg %>%
  filter(category == 'Lichen' |
           category == 'Liverwort' |
           category == 'Moss')

# Summarize AKVEG nonvascular data to functional groups
nonvascular_akveg = nonvascular_akveg %>%
  inner_join(functional_groups, by = 'name_accepted') %>%
  group_by(project, site_code, veg_observe_date, cover_type, functional_group) %>%
  summarize(cover = sum(cover)) %>%
  select(project, site_code, veg_observe_date, cover_type, functional_group, cover) %>%
  rename(name_accepted = functional_group) %>%
  mutate(cover = if_else(cover > 100, 100, cover))

# Control duplicate errors in AKVEG vascular data
vascular_akveg = vascular_akveg %>%
  group_by(project, site_code, veg_observe_date, cover_type, name_accepted) %>%
  summarize(cover = max(cover))

# Split AKVEG vascular data into tree and non-tree data
tree_akveg = vascular_akveg %>%
  filter(name_accepted == 'Picea sitchensis' |
           name_accepted == 'Picea ×lutzii' |
           name_accepted == 'Picea glauca' |
           name_accepted == 'Picea mariana' |
           name_accepted == 'Larix laricina' |
           name_accepted == 'Betula kenaica' |
           name_accepted == 'Betula neoalaskana' |
           name_accepted == 'Populus tremuloides' |
           name_accepted == 'Populus trichocarpa' |
           name_accepted == 'Populus balsamifera')
nontree_akveg = vascular_akveg %>%
  filter((name_accepted != 'Picea sitchensis' &
            name_accepted != 'Picea ×lutzii' &
            name_accepted != 'Picea glauca' &
            name_accepted != 'Picea mariana' &
            name_accepted != 'Larix laricina' &
            name_accepted != 'Betula kenaica' &
            name_accepted != 'Betula neoalaskana' &
            name_accepted != 'Populus tremuloides' &
            name_accepted != 'Populus trichocarpa' &
            name_accepted != 'Populus balsamifera') &
           cover >= 3)

# Combine vascular and nonvascular AKVEG cover data
cover_akveg_formatted = rbind(tree_akveg, nontree_akveg, nonvascular_akveg)

#### COMBINE AND EXPORT DATA
####------------------------------

# Combine FIA and AKVEG data
cover_all = rbind(cover_fia, cover_akveg_formatted)

# Join analysis codes
cover_all = cover_all %>%
  left_join(codes_analysis, by = 'name_accepted')

# Export data
write.csv(cover_all, file = output_cover, fileEncoding = 'UTF-8', row.names = FALSE)