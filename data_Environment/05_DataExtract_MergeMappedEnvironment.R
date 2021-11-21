# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Merge mapped environment data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-10-12
# Usage: Script should be executed in R 4.0.0+.
# Description: "Merge mapped environment data" merges the mapped environmental covariates extracted to points from the FIA Interior database and the AKVEG database.
# ---------------------------------------------------------------------------

# Set root directory
drive = 'N:'
root_folder = 'ACCS_Work'

# Define input folders
data_folder = paste(drive,
                    root_folder,
                    'Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input',
                    sep = '/')

# Define the input files
input_akveg = paste(data_folder,
                    'environment',
                    'environment_mapped_akveg.csv',
                    sep = '/')
input_fia = paste(data_folder,
                  'environment',
                  'environment_mapped_fia.csv',
                  sep = '/')

# Define output file
output_file = paste(data_folder,
                    'environment',
                    'environment_mapped_nab.csv',
                    sep = '/')

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

# Read data into dataframes
env_akveg = read.csv(input_akveg, encoding = 'UTF-8')
env_fia = read.csv(input_fia, encoding = 'UTF-8')

# Combine FIA and AKVEG data
env_all = rbind(env_fia, env_akveg)

# Export data
write.csv(env_all, file = output_file, fileEncoding = 'UTF-8', row.names = FALSE)