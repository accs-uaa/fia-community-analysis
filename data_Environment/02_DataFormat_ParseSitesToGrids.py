# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Parse Sites to Grids
# Author: Timm Nawrocki
# Last Updated: 2021-10-11
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Parse Sites to Grids" prepares a table of point data for feature extraction by selecting appropriate raster cells based on cell size and splits raster points by major grids.
# ---------------------------------------------------------------------------

# Import packages
import arcpy
import os
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import format_site_data
from package_GeospatialProcessing import parse_site_data

# Set root directory
drive = 'N:/'
root_folder = 'ACCS_Work'

# Define data folder
data_folder = os.path.join(drive, root_folder, 'Projects/VegetationEcology/FIA_Community_Analysis/Data')

# Create parsed folders within data folder if it does not already exist
parsed_akveg = os.path.join(data_folder, 'Data_Input/sites/parsed_akveg')
parsed_fia = os.path.join(data_folder, 'Data_Input/sites/parsed_fia')
if os.path.exists(parsed_akveg) == 0:
    os.mkdir(parsed_akveg)
if os.path.exists(parsed_fia) == 0:
    os.mkdir(parsed_fia)

# Define work environment
work_geodatabase = os.path.join(drive, root_folder,
                                'Projects/VegetationEcology/FIA_Community_Analysis/Data/FIA_Community_Analysis.gdb')

# Define input datasets
sites_akveg = os.path.join(work_geodatabase, 'sites_akveg_AKALB')
sites_fia = os.path.join(work_geodatabase, 'sites_fia_AKALB')
grid_major = os.path.join(work_geodatabase, 'NorthAmericanBeringia_GridIndex_Major_400km_Selected')
area_of_interest = os.path.join(data_folder, 'Data_Input/northAmericanBeringia_ModelArea.tif')

# Define output point feature class
formatted_akveg = os.path.join(work_geodatabase, 'sites_akveg_formatted')
formatted_fia = os.path.join(work_geodatabase, 'sites_fia_formatted')

# Define input and output lists
input_lists = [[sites_akveg, area_of_interest], [sites_fia, area_of_interest]]
output_lists = [[formatted_akveg], formatted_fia]
output_folders = [parsed_akveg, parsed_fia]

# Loop through input lists and perform processing steps
count = 1
for input_list in input_lists:
    # Define output list
    output_list = output_lists[count - 1]

    # Format site data if it does not already exist
    if arcpy.Exists(output_list[0]) == 0:
        # Define input and output arrays to format site data
        format_inputs = input_list
        format_outputs = output_list

        # Define cell_size
        if output_list[0] == formatted_akveg:
            cell_size = 2
        else:
            cell_size = 20

        # Create key word arguments
        format_kwargs = {'work_geodatabase': work_geodatabase,
                         'cell_size': cell_size,
                         'input_array': format_inputs,
                         'output_array': format_outputs
                         }

        # Format site data
        print(f'Formatting site data...')
        arcpy_geoprocessing(format_site_data, **format_kwargs)
        print('----------')

    else:
        print('Formatted data already exists.')
        print('----------')

    # Parse site data to grids with a search cursor
    parsed_folder = output_folders[count-1]
    parsed_inputs = [output_list[0], grid_major]
    with arcpy.da.SearchCursor(grid_major, ['Major']) as cursor:
        # Iterate through each grid in major grid
        for row in cursor:
            # Identify grid name
            grid_name = row[0]

            # Define output array
            output_shapefile = os.path.join(parsed_folder, grid_name + '.shp')
            parsed_outputs = [output_shapefile]

            # Create key word arguments
            parsed_kwargs = {'work_geodatabase': work_geodatabase,
                             'grid_name': grid_name,
                             'input_array': parsed_inputs,
                             'output_array': parsed_outputs
                             }

            # Parse site data
            print(f'Parsing site data for grid {grid_name}...')
            arcpy_geoprocessing(parse_site_data, **parsed_kwargs)
            print('----------')

    # Increase counter
    count += 1
