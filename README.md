# Cropecosg
This repository gathers script to crop ECOCLIMAP-SG covers and paramters. 
## Requirements
* gfortran compiler
python libraires
* os
* f90nml
* pandas
* subprocess
* sys
* gdal 
* rasterio
* numpy

## Repository contents
It contains 2 folders that the user does not need to edit anything in:
 * Crop_paramters: Contains all the function used in the different python scripts and notebooks
 * old_scripts: Contains scripts that have been combined or modified to create the Crop_paramters files
 * Crop_cover(.ipynb/.py): is used to crop ECOCLIMAP-SG cover type (ECOCLIMAP-SG uncompressed file should work but it was only tested with cover)
 * Crop_paramter (.ipynb/.py): is used to crop ECOLIMAP_SG albedo and LAI files (any compressed file should work too)
To know whether a file is compressed the hdr file should have the line (compress:1)


## Crop_cover
### How to use it
The Crop_cover notebook (.ipynb) or script (.py) are the same just on a different format.
The user needs to set in the notebook or script
- input_ecosgcoverpath: (Local copy of the original ECOCLIMAP-SG is located  ex:'/data/gbessardon/ECOCLIMAP-SG/COVER/ecosg_final_map.dir')
- output_path : the folder where the output will be created (not the full path ex: Cropped_cover) 
- xmin Minimum longitude in degrees
- xmax Maximum longitude in degrees
- ymin Minimum latitude in degrees
- ymax Maximum latitude in degrees

Run the script and the ouput should be located $wheremyscriptislocated/$output_path (ex:/data/gbessardon/Crop_ecosg/Cropped_cover)

### How it works 
The Crop_cover imports the different libraries and script, reads the user inputs.
(.hdr) ECOCLIMAP-SG file format is not readable in python so the function  "Compress_ecosg.Create_readable_ecosg_cover" creates temporary python readable (.hdr) file and a soft link to the local ECOCLIMAP-SG (.dir) file
The function "Crop_parameters.Crop_data_rasterio" crops the file and saves it in the output_path (the function is contained in the Crop_paramters/Crop_paramters.py file)
The function "Compress_ecosg.create_ecosgcoverhdr" converts the python readable (.hdr) file into a ECOCLIMAP-SG format (.hdr)
(the function is contained in the Crop_paramters/Compress_ecosg.py file)
clears the folder from the temporarly created file


## Crop_paramter
### How to use it

The Crop_parameter notebook (.ipynb) or script (.py) are the same just on a different format.
The user needs to set in the notebook or script:
- input_directory (Local copy of the original ECOCLIMAP-SG parameter file ex: '/data/gbessardon/ECOCLIMAP-SG/LAI/300M')
- input_ecosgcoverpath: (Local copy of the original ECOCLIMAP-SG is located  ex:'/data/gbessardon/ECOCLIMAP-SG/COVER/ecosg_final_map.dir')
- output_dir : the folder where the output will be created (not the full path ex: Cropped_compressed_LAI) 
- xmin Minimum longitude in degrees
- xmax Maximum longitude in degrees
- ymin Minimum latitude in degrees
- ymax Maximum latitude in degrees
Optional:
- Uncompress_dir: folder where the uncompressed files will be located 
- Crop_dir: folder where the cropped uncompressed files will be located

Run the script and the ouput should be located $wheremyscriptislocated/$output_dir (ex:/data/gbessardon/Crop_ecosg/Cropped_compressed_LAI)

### How it works 
The Crop_parameter imports the different libraries and script, reads the user inputs, defines the alltogether function which crops one file
Makes a list of all the files to crop
the function  "Compress_ecosg.Create_readable_ecosg_cover" creates temporary python readable (.hdr) file and a soft link to the local ECOCLIMAP-SG (.dir) file
The script then loops over all the files to crop.
The alltogether function does the cropping for each file it calls:
- Uncompress.Uncompressfile which uncompress the file saves it in the $Uncompress_dir and extract the files data this this script compiles and calls Uncompressecosg.F90 which comes from the script provided by the CNRM (the function is contained in the Crop_paramters/Uncompress.py file)
- The function "Crop_parameters.Crop_data_rasterio" crops the uncompressed file and saves it in the $Crop_dir (the function is contained in the Crop_paramters/Crop_paramters.py file)
-  os.remove(Ucfn) removes the Uncompressed .dir file (to save memory)
-  Compress_ecosg.Compressfile Compress the cropped file and create the associated hdr file in the output_dir (the function is contained in the Crop_paramters/Compress_ecosg.py file)
-  os.remove(Croppedfn) removes the uncompressed .dir cropped file