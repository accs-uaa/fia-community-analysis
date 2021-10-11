# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Prepare Lichen and Bryophyte Functional Groups
# Author: Timm Nawrocki, Alaska Center for Conservation Science
# Last Updated: 2021-05-01
# Usage: Script should be executed in R 4.0.0+.
# Description: "Prepare Lichen and Bryophyte Functional Groups" creates a reference table of lichen and bryophyte functional groups by accepted genera.
# ---------------------------------------------------------------------------

# Set root directory
drive = 'N:'
root_folder = 'ACCS_Work'

# Define input folders
data_folder = paste(drive,
                    root_folder,
                    'Projects/VegetationEcology/FIA_Community_Analysis/Data/Data_Input',
                    sep = '/')

# Define output file
output_file = paste(data_folder,
                    'reference',
                    'nonvascular_functional_groups.csv',
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
query_nonvascular = 'SELECT taxon_accepted.name_accepted as name_accepted
    , hierarchy.genus_accepted as genus_accepted
    , family.family as family
FROM taxon_accepted
    LEFT JOIN hierarchy ON taxon_accepted.hierarchy_id = hierarchy.hierarchy_id
    LEFT JOIN family ON hierarchy.family_id = family.family_id
WHERE hierarchy.category_id IN (6, 7, 10)
AND taxon_accepted.level_id != 6;'

# Read PostgreSQL taxonomy tables into dataframes
nonvascular_table = as_tibble(dbGetQuery(akveg_connection, query_nonvascular))

# Set all to other
nonvascular_groups = nonvascular_table %>%
  mutate(functional_group = 'Other')

# Add Functional Group: Lichenicolous Fungi
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Abrothallaceae' |
                                      family == 'Agyriaceae' |
                                      family == 'Adelococcaceae' |
                                      family == 'Athropyreniaceae' |
                                      family == 'Bionectriaceae' |
                                      family == 'Carcinomycetaceae' |
                                      family == 'Clavariaceae' |
                                      family == 'Cordieritidaceae' |
                                      family == 'Decampiaceae' |
                                      family == 'Didymellaceae' |
                                      family == 'Helicogoniaceae' |
                                      family == 'Hyaloscyphaceae' |
                                      genus_accepted == 'Arthophacopsis' |
                                      genus_accepted == 'Bachmanniomyces' |
                                      genus_accepted == 'Endococcus' |
                                      genus_accepted == 'Epicladonia' |
                                      genus_accepted == 'Illosporium' |
                                      genus_accepted == 'Intralichen' |
                                      genus_accepted == 'Lichenopuccinia' |
                                      genus_accepted == 'Lichenosticta' |
                                      genus_accepted == 'Monodyctis' |
                                      genus_accepted == 'Phaeosporobolus' |
                                      genus_accepted == 'Piccolia' |
                                      genus_accepted == 'Refractohilum' |
                                      genus_accepted == 'Rhymbocarpus' |
                                      genus_accepted == 'Roselliniella' |
                                      genus_accepted == 'Sclerococcum' |
                                      genus_accepted == 'Scutula' |
                                      family == 'Lichenoconiaceae' |
                                      family == 'Lichenotheliaceae' |
                                      family == 'Microthyriaceae' |
                                      family == 'Mycosphaerellaceae' |
                                      family == 'Mytilinidiaceae' |
                                      family == 'Nectriaceae' |
                                      family == 'Nitschkiaceae' |
                                      family == 'Polycoccaceae' |
                                      family == 'Pseudoperisporiaceae' |
                                      family == 'Sphinctrinaceae' |
                                      family == 'Spirographaceae' |
                                      family == 'Xanthopyreniaceae',
                                    'Lichenicolous Fungi',
                                    functional_group))

# Add Functional Group: Crustose Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Acarosporaceae' |
                                      family == 'Arctomiaceae' |
                                      family == 'Arthoniaceae' |
                                      family == 'Arthopyreniaceae' |
                                      family == 'Arthrorhaphidaceae' |
                                      family == 'Baeomycetaceae' |
                                      family == 'Biatorellaceae' |
                                      family == 'Brigantiaceae' |
                                      family == 'Caliciaceae' |
                                      family == 'Carbonicolaceae' |
                                      family == 'Catillariaceae' |
                                      family == 'Coccotremataceae' |
                                      family == 'Coenogoniaceae' |
                                      family == 'Cystocoleaceae' |
                                      family == 'Dactylosporaceae' |
                                      family == 'Elixiaceae' |
                                      family == 'Epigloeaceae' |
                                      family == 'Fuscideaceae' |
                                      family == 'Gomphillaceae' |
                                      family == 'Graphidaceae' |
                                      family == 'Gyalectaceae' |
                                      family == 'Haematommataceae' |
                                      family == 'Harpidiaceae' |
                                      family == 'Helocarpaceae' |
                                      family == 'Hymeneliaceae' |
                                      genus_accepted == 'Aspilidea' |
                                      genus_accepted == 'Botryolepraria' |
                                      genus_accepted == 'Cercidospora' |
                                      genus_accepted == 'Corticifraga' |
                                      genus_accepted == 'Myochroidea' |
                                      genus_accepted == 'Puttea' |
                                      family == 'Koerberiaceae' |
                                      family == 'Lecanoraceae' |
                                      family == 'Lecideaceae' |
                                      family == 'Leprocaulaceae' |
                                      family == 'Lichinaceae' |
                                      family == 'Lopadiaceae' |
                                      family == 'Megasporaceae' |
                                      family == 'Microascaceae' |
                                      family == 'Microcaliciaceae' |
                                      family == 'Ochrolechiaceae' |
                                      family == 'Opegraphaceae' |
                                      family == 'Ophioparmaceae' |
                                      family == 'Pertusariaceae' |
                                      family == 'Phlyctidaceae' |
                                      family == 'Pilocarpaceae' |
                                      family == 'Placynthiaceae' |
                                      family == 'Porinaceae' |
                                      family == 'Psilolechiaceae' |
                                      family == 'Psoraceae' |
                                      family == 'Protothelenellaceae' |
                                      family == 'Pycnoraceae' |
                                      family == 'Ramalinaceae' |
                                      family == 'Rhizocarpaceae' |
                                      family == 'Roccellaceae' |
                                      family == 'Ropalosporaceae' |
                                      family == 'Sagiolechiaceae' |
                                      family == 'Schaereriaceae' |
                                      family == 'Scoliciosporaceae' |
                                      family == 'Sporastatiaceae' |
                                      family == 'Squamarinaceae' |
                                      family == 'Stictidaceae' |
                                      family == 'Strangosporaceae' |
                                      family == 'Tephromelataceae' |
                                      family == 'Thelenellacaea' | # Misspelled family name
                                      family == 'Thelenellaceae' |
                                      family == 'Thelocarpaceae' |
                                      family == 'Thrombiaceae' |
                                      family == 'Trapeliaceae' |
                                      family == 'Umbilicariaceae' |
                                      family == 'Vahliellaceae' |
                                      family == 'Variolariaceae' |
                                      family == 'Verrucariaceae' |
                                      family == 'Xylographaceae',
                                    'Crustose Lichen',
                                    functional_group))

# Add Functional Group: N-fixing Foliose Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Lobariaceae' |
                                      family == 'Pannariaceae' |
                                      family == 'Peltigeraceae' |
                                      family == 'Nephromataceae' |
                                      genus_accepted == 'Dermatocarpon',
                                    'N-fixing Foliose Lichen',
                                    functional_group))

# Add Functional Group: Foliose Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Coccocarpiaceae' |
                                      family == 'Collemataceae' |
                                      family == 'Parmeliaceae' |
                                      family == 'Physciaceae' |
                                      genus_accepted == 'Polyblastidium' |
                                      family == 'Massalongiaceae' |
                                      family == 'Pannariaceae',
                                    'Foliose Lichen',
                                    functional_group))

# Add Functional Group: N-fixing Fruticose Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Stereocaulaceae',
                                    'N-fixing Fruticose Lichen',
                                    functional_group))

# Add Functional Group: Fruticose Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Cladoniaceae' |
                                      family == 'Icmadophilaceae' |
                                      genus_accepted == 'Bunodophoron' |
                                      family == 'Tremellaceae',
                                    'Fruticose Lichen',
                                    functional_group))

# Add Functional Group: Other Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Coniocybaceae' |
                                      family == 'Hygrophoraceae' |
                                      family == 'Malmideaceae' |
                                      family == 'Mycocaliciaceae' |
                                      family == 'Naetrocymbaceae',
                                    'Other Lichen',
                                    functional_group))

# Add Functional Group: Orange Lichens
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(genus_accepted == 'Rusavkia' |
                                      genus_accepted == 'Xanthomendoza' |
                                      family == 'Candelariaceae' |
                                      family == 'Chrysotrichaceae' |
                                      genus_accepted == 'Ramboldia' |
                                      family == 'Teloschistaceae',
                                    'Orange Lichen',
                                    functional_group))

# Add Functional Group: Forage Lichen
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(name_accepted == 'Cladonia arbuscula' |
                                      name_accepted == 'Cladonia arbuscula ssp. arbuscula' |
                                      name_accepted == 'Cladonia arbuscula ssp. mitis' |
                                      name_accepted == 'Cladonia ciliata' |
                                      name_accepted == 'Cladonia portentosa' |
                                      name_accepted == 'Cladonia portentosa ssp. pacifica' |
                                      name_accepted == 'Cladonia rangiferina' |
                                      name_accepted == 'Cladonia stellaris' |
                                      name_accepted == 'Cladonia stygia' |
                                      genus_accepted == 'Alectoria' |
                                      genus_accepted == 'Bryoria' |
                                      genus_accepted == 'Bryocaulon' |
                                      genus_accepted == 'Thamnolia' |
                                      genus_accepted == 'Cetraria' |
                                      genus_accepted == 'Flavocetraria' |
                                      genus_accepted == 'Masonhalea' |
                                      genus_accepted == 'Sphaerophorus',
                                    'Forage Lichen',
                                    functional_group))

# Add Functional Group: N-fixing Feathermoss
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Hylocomiaceae',
                                    'N-fixing Feathermoss',
                                    functional_group))

# Add Functional Group: Feathermoss
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Brachytheciaceae' |
                                      family == 'Climaciaceae' |
                                      family == 'Helodiaceae' |
                                      family == 'Hypnaceae' |
                                      family == 'Hypopterygiaceae' |
                                      family == 'Lembophyllaceae' |
                                      family == 'Leskeaceae' |
                                      family == 'Leucodontaceae' |
                                      family == 'Thuidiaceae' |
                                      family == 'Neckeraceae' |
                                      family == 'Rhytidiaceae',
                                    'Feathermoss',
                                    functional_group))

# Add Functional Group: Sphagnum
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Sphagnaceae',
                                    'Sphagnum',
                                    functional_group))

# Add Functional Group: Turf Moss
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Bartramiaceae' |
                                      family == 'Bruchiaceae' |
                                      family == 'Bryaceae' |
                                      family == 'Bryoxiphiaceae' |
                                      family == 'Buxbaumiaceae' |
                                      family == 'Catoscopiaceae' |
                                      family == 'Dicranaceae' |
                                      family == 'Ditrichaceae' |
                                      family == 'Encalyptaceae' |
                                      family == 'Entodontaceae' |
                                      family == 'Fissidentaceae' |
                                      family == 'Flexitrichaceae' |
                                      family == 'Funariaceae' |
                                      family == 'Grimmiaceae' |
                                      family == 'Hedwigiaceae' |
                                      family == 'Leucobryaceae' |
                                      family == 'Mniaceae' |
                                      family == 'Myriniaceae' |
                                      family == 'Orthotrichaceae' |
                                      family == 'Polytrichaceae' |
                                      family == 'Pottiaceae' |
                                      family == 'Ptychomitriaceae' |
                                      family == 'Pylaisiaceae' |
                                      family == 'Rhabdoweisiaceae' |
                                      family == 'Saelaniaceae' |
                                      family == 'Scouleriaceae' |
                                      family == 'Seligeriaceae' |
                                      family == 'Tetraphidaceae' |
                                      family == 'Timmiaceae',
                                    'Turf Moss',
                                    functional_group))

# Add Functional Group: Other Moss
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Amblystegiaceae' |
                                      family == 'Andreaeaceae' |
                                      family == 'Andreaeobryaceae' |
                                      family == 'Aulacomniaceae' |
                                      family == 'Calliergonaceae' |
                                      family == 'Fontinalaceae' |
                                      family == 'Hookeriaceae' |
                                      family == 'Meesiaceae' |
                                      family == 'Mniaceae' |
                                      family == 'Oedipodiaceae' |
                                      family == 'Plagiotheciaceae' |
                                      family == 'Pterigynandraceae' |
                                      family == 'Schistostegaceae' |
                                      family == 'Splachnaceae' |
                                      family == 'Takakiaceae',
                                    'Other Moss',
                                    functional_group))

# Add Functional Group: Thalloid Liverwort
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Aneuraceae' |
                                      family == 'Aytoniaceae' |
                                      family == 'Blasiaceae' |
                                      family == 'Calyculariaceae' |
                                      family == 'Cleveaceae' |
                                      family == 'Conocephalaceae' |
                                      family == 'Fossombroniaceae' |
                                      family == 'Marchantiaceae' |
                                      family == 'Metzgeriaceae' |
                                      family == 'Moerckiaceae' |
                                      family == 'Pelliaceae' |
                                      family == 'Pylaisiadelphaceae' |
                                      family == 'Ricciaceae',
                                    'Thalloid Liverwort',
                                    functional_group))

# Add Functional Group: Liverwort
nonvascular_groups = nonvascular_groups %>%
  mutate(functional_group = if_else(family == 'Acroboldaceae' |
                                      family == 'Adelanthaceae' |
                                      family == 'Anastrophyllaceae' |
                                      family == 'Antheliaceae' |
                                      family == 'Arnelliaceae' |
                                      family == 'Blepharostomataceae' |
                                      family == 'Calypogeiaceae' |
                                      family == 'Cephaloziaceae' |
                                      family == 'Cephaloziellaceae' |
                                      family == 'Endogemmataceae' |
                                      family == 'Frullaniaceae' |
                                      family == 'Geocalycaceae' |
                                      family == 'Gymnomitriaceae' |
                                      family == 'Gyrothyraceae' |
                                      family == 'Haplomitriaceae' |
                                      family == 'Herbertaceae' |
                                      family == 'Jungermanniaceae' |
                                      family == 'Lejeuneaceae' |
                                      family == 'Lepidoziaceae' |
                                      family == 'Lophocoleaceae' |
                                      family == 'Myliaceae' |
                                      family == 'Plagiochilaceae' |
                                      family == 'Pleuroziaceae' |
                                      family == 'Porellaceae' |
                                      family == 'Ptilidiaceae' |
                                      family == 'Radulaceae' |
                                      family == 'Scapaniaceae' |
                                      family == 'Solenostomataceae' |
                                      family == 'Treubiaceae',
                                    'Liverwort',
                                    functional_group))

# Export data
write.csv(nonvascular_groups, file = output_file, fileEncoding = 'UTF-8', row.names = FALSE)