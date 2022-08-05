#!/usr/bin/env python
# coding: utf-8

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


# In[4]:


input_ecosgcoverpath='/data/gbessardon/ECOCLIMAP-SG/COVER/ecosg_final_map.dir'
output_path='Cropped_cover_small'
xmin=-35
xmax=15
ymin=20
ymax=60


# In[5]:


(dictp,dictecosg,outfn,fnlink)=Compress_ecosg.Create_readable_ecosg_cover(input_ecosgcoverpath)
Crop_parameters.Crop_data_rasterio(fnlink,output_path,xmin, ymin, xmax, ymax)
outhdr=os.path.join(os.getcwd(),output_path,outfn.split('/')[-1])
Compress_ecosg.create_ecosgcoverhdr(outhdr,outhdr)
os.unlink(fnlink)
os.remove(outfn)
os.remove(outhdr.replace('.hdr','.dir.aux.xml'))


# In[ ]:




