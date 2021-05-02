# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Adjudicate FIA Cover Data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-05-01
# Usage: Script should be executed in R 4.0.0+.
# Description: "Adjudicate FIA Cover Data" merges cover values from multiple layers, finds plot level means, and reconciles nomenclature to the accepted names of the AKVEG taxonomic standard.
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

# Define input USDA Plants Database codes
plants_file = paste(data_folder,
                    'reference',
                    'speciesPlants_03062018.xlsx',
                    sep = '/')

# Define output file
output_cover = paste(data_folder,
                     'vegetation',
                     'cover_fia.csv',
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

# Read plants table into data frame
plants_codes = read_excel(plants_file, sheet = 'speciesPlants')

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

# Read cover data into tibbles
vascular_table = as_tibble(dbGetQuery(fia_connection, query_vascular))
nonvascular_table = as_tibble(dbGetQuery(fia_connection, query_nonvascular))

# Summarize vascular cover by subplot
vascular_subplot = vascular_table %>%
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

# Summarize cover by plot and add metadata
vascular_plot = vascular_subplot %>%
  group_by(site_code, veg_observe_date, name_original_code) %>%
  summarize(cover = mean(cover)) %>%
  left_join(plants_codes, by = c('name_original_code' = 'codePLANTS')) %>%
  rename(name_original = namePLANTS) %>%
  left_join(taxa_adjudicated, by = c('name_original' = 'name_adjudicated')) %>%
  left_join(taxa_accepted, by = 'accepted_id') %>%
  mutate(project = 'FIA Interior') %>%
  mutate(cover_type = 'total cover')

# Adjudicate accepted species names
vascular_plot = vascular_plot %>%
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

# Identify functional groups
nonvascular_subplot = nonvascular_table %>%
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

# Summarize nonvascular cover by plot
nonvascular_plot = nonvascular_subplot %>%
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

# Combine vascular and nonvascular cover
cover_plot = rbind(vascular_plot, nonvascular_plot)

# Export data
write.csv(cover_plot, file = output_cover, fileEncoding = 'UTF-8', row.names = FALSE)