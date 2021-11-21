sites_file = 'N:/ACCS_Work/Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input/sites/sites_nab.xlsx'
cover_file = 'N:/ACCS_Work/Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input/vegetation/cover_nab.csv'
env_file = 'N:/ACCS_Work/Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input/environment/environment_nab.csv'
mapped_file = 'N:/ACCS_Work/Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input/environment/environment_mapped_nab.csv'

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

sites_data = read_xlsx(sites_file, sheet = 'sites_nab')
cover_data = read.csv(cover_file, encoding = 'UTF-8')
env_data = read.csv(env_file, encoding = 'UTF-8')
mapped_data = read.csv(mapped_file, encoding = 'UTF-8')

sites_sites = sites_data %>%
  select(site_code) %>%
  distinct()

cover_sites = cover_data %>%
  select(site_code) %>%
  distinct()

env_sites = env_data %>%
  select(site_code) %>%
  distinct()

mapped_sites = mapped_data %>%
  select(site_code) %>%
  distinct()

env_missing = sites_sites %>%
  anti_join(env_sites, by = 'site_code')

cover_missing = sites_sites %>%
  anti_join(cover_sites, by = 'site_code')

mapped_missing = sites_sites %>%
  anti_join(mapped_sites, by = 'site_code')





