# Cropecosg
This repository gathers script to crop ECOCLIMAP-SG covers and paramters. 
It contains 2 folders that the user does not need to edit anything in:
 * Crop_paramters: Contains all the function used in the different python scripts and notebook
 * old_scripts: Contains scripts that have been combined or modified to create the Crop_paramters files

Crop_cover is use to crop ECOCLIMAP-SG cover type (
Crop_paramter is use to crop ECOLIMAP_SG albedo and LAI files (any compressed file should work but only tested on LAI and Albedo AVS files yet. Require further testing to make sure the fact is correct)
To know whether a file is compressed the hdr file should have the line (compress:1)