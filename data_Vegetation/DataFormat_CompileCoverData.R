# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Adjudicate FIA Cover Data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-04-30
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
                    'FIA',
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
query_cover = 'SELECT P2VEG_SUBPLOT_SPP.PLOT as plot
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

# Read cover data into tibble
cover_table = as_tibble(dbGetQuery(fia_connection, query_cover))

# Summarize cover by subplot
cover_subplot = cover_table %>%
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
cover_plot = cover_subplot %>%
  group_by(site_code, veg_observe_date, name_original_code) %>%
  summarize(cover = mean(cover)) %>%
  mutate(veg_observe_date = ymd(veg_observe_date)) %>%
  left_join(plants_codes, by = c('name_original_code' = 'codePLANTS')) %>%
  rename(name_original = namePLANTS) %>%
  left_join(taxa_adjudicated, by = c('name_original' = 'name_adjudicated')) %>%
  left_join(taxa_accepted, by = 'accepted_id') %>%
  mutate(project = 'FIA Interior') %>%
  mutate(cover_type = 'total cover') %>%
  select(project, site_code, veg_observe_date, cover_type, name_original_code, name_original, name_accepted, cover)

# Export data
write.csv(cover_plot, file = output_cover, fileEncoding = 'UTF-8', row.names = FALSE)