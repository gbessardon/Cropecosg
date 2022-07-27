#!/usr/bin/env python
# coding: utf-8

# # Import Functions

# In[1]:


import os
import f90nml
import pandas as pd
import subprocess
import sys


# In[2]:


sys.path.insert(0,os.path.join(os.getcwd(),'Crop_parameters'))


# In[3]:


import Uncompress
import Crop_parameters
import Compress_ecosg


# # Define all functions

# In[4]:


input_directory='/data/gbessardon/ECOCLIMAP-SG/LAI/300M'
input_ecosgcoverpath='/data/gbessardon/ECOCLIMAP-SG/COVER/ecosg_final_map.dir'
Uncompress_dir='Uncompressed_small_lai' 
xmin=-35
xmax=15
ymin=20
ymax=60
Crop_dir='Cropped_small_LAI'
output_dir='Cropped_compressed_small_LAI'


# # Function that uncompress crop then recompress a LAI or albedo file

# In[5]:


def alltogether(fn,fncoverlink,output_dir,xmin,xmax,ymin,ymax,
                Uncompress_dir='Uncompressed_LAI',Cropped_dir='Cropped_dir'):
        Uncompress.Uncompressfile(Uncompress_dir,fn)
        Ucfn=os.path.join(os.getcwd(),Uncompress_dir,fn.split('/')[-1])
        Crop_parameters.Crop_data_rasterio(Ucfn,Crop_dir,xmin, ymin, xmax, ymax)
        os.remove(Ucfn)
        Croppedfn=os.path.join(os.getcwd(),Crop_dir,fn.split('/')[-1])
        Compress_ecosg.Compressfile(output_dir,Croppedfn,fncoverlink)
        os.remove(Croppedfn)
        outputfn=os.path.join(os.getcwd(),output_dir,fn.split('/')[-1])
        return(outputfn)
    


# In[6]:


# list files to uncompress
listdirfiles=[os.path.join(input_directory,f) for f in os.listdir(input_directory) if f.endswith('.dir')]


# In[7]:


# Import ECO-SG for the compression
(dictp,dictecosg,outfn,fncoverlink)=Compress_ecosg.Create_readable_ecosg_cover(input_ecosgcoverpath)


# In[ ]:


[alltogether(fn,fncoverlink,output_dir,xmin,xmax,ymin,ymax,Uncompress_dir) for fn in listdirfiles]

