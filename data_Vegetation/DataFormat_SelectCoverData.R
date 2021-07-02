# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Select Arctic-boreal cover data
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-07-01
# Usage: Script should be executed in R 4.0.0+.
# Description: "Select Arctic-boreal cover data" selects the cover data that corresponds to a clipped sites table. Site table should be clipped to the North American Beringia feature class.
# ---------------------------------------------------------------------------

# Set root directory
drive = 'N:'
root_folder = 'ACCS_Work'

# Define input folders
data_folder = paste(drive,
                    root_folder,
                    'Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input',
                    sep = '/')

# Define input clipped sites file
sites_file = paste(data_folder,
                    'sites',
                    'sites_nab.xlsx',
                    sep = '/')

# Define input cover file
cover_file = paste(data_folder,
                   'vegetation',
                   'cover_all.csv',
                   sep = '/')

# Define output cover file
cover_output = paste(data_folder,
                     'vegetation',
                     'cover_nab.csv',
                     sep = '/')

# Import required libraries
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(tidyr)

# Read tables into data frames
site_data = read_excel(sites_file, sheet = 'sites_nab')
cover_data = read.csv(cover_file, encoding = 'UTF-8')

# Restrict sites data to site_code
sites = site_data %>%
  select(site_code)

# Join site codes to cover data
cover_select = cover_data %>%
  inner_join(sites, by = 'site_code')

# Export selected cover data
write.csv(cover_select, file = cover_output, fileEncoding = 'UTF-8', row.names = FALSE)