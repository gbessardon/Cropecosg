#!/usr/bin/env python
# coding: utf-8

# # Declare libraries

# In[1]:


import os
import f90nml
import pandas as pd
import subprocess
import numpy as np
import rasterio
from rasterio.windows import Window
from rasterio.merge import merge
import pycrs
import shutil


# # Declare Directories

# In[2]:


input_directory='/data/gbessardon/laialebdomodis/Uncompress/Uncompressed_LAI/'
input_ecosgcoverpath='/data/gbessardon/ECOCLIMAP-SG/COVER/ecosg_final_map.dir'
output_end='Compressed_LAI' 


# # Decalare functions

# ## Function to Create the outputdir

# In[3]:


def Createoutputdir(outputdir):
    outpath=os.path.join(os.getcwd(),outputdir)
    if not os.path.isdir(outpath):
        os.mkdir(outpath)
    return outpath


# ## Re-write Uncompressed .hdr file into ECOSG .hdr format

# In[4]:


def create_ecosghdr(fnhdr,outfn):
    NAMELISTpath=os.path.join(os.getcwd(),'Namelist.nml')
    nml = f90nml.read(NAMELISTpath)
    f=open(fnhdr,'r')
    lines=f.readlines()
    characteristics=[l.split(' ')[0] for l in lines]
    value=[l.strip().replace('  ',' ').split(' ')[1] for l in lines]
    dicti ={'characteristics':characteristics, 'value':value}
    dictuncompress=pd.DataFrame(dicti,index=characteristics).to_dict()['value']
    dictp={'ECOCLIMAP' : '',
            'nodata' : '0',
            'north' : '80.',
            'south' : '-60.',
            'west' : '-180.',
            'east' : '180.',
            'rows' : '50400',
            'cols' : '129600',
            'fact' : '10',
            'compress' : '1',
            'recordtype' : 'integer 16 bytes' }
    dictp['nodata']=dictuncompress['nodata']
    dictp['rows']=dictuncompress['nrows']
    dictp['cols']=dictuncompress['ncols']
    dictp['north']=dictuncompress['ULYMAP']
    dictp['south']=str(float(dictuncompress['ULYMAP'])-(float(dictuncompress['nrows'])*float(dictuncompress['YDIM'])))
    dictp['west']=dictuncompress['ULXMAP']
    dictp['east']=str(float(dictuncompress['ULXMAP'])+(float(dictuncompress['ncols'])*float(dictuncompress['XDIM'])))
    with open(outfn,'w+') as f:
        for i,v in dictp.items():
            f.write(i+': '+v+'\n')
        f.close()
    return(dictp,dictuncompress)


# ## Create python readable .hdr 

# In[5]:


def create_readablehdr(fnhdr,outfn):

    dictecosg=pd.read_csv(fnhdr,delimiter=":",
                      names=['characteristics','value'],
                      index_col='characteristics',
                     skiprows=1).to_dict()['value']

    dictp={'nrows' : '16800',
           'ncols' : '43200',
           'nodata' : '0',
           'nbands' : '1',
           'nbits'  : '8',
           'ULXMAP' :    '-180.00000000000000',
           'ULYMAP' :      '79.9999999999986',
           'XDIM'  :        '0.00277777777777',
           'YDIM'  :        '0.00277777777777'}
    
    dictp['nodata']=dictecosg['nodata']
    dictp['nrows']=dictecosg['rows']
    dictp['ncols']=dictecosg['cols']
    dictp['ULXMAP']=dictecosg['west']
    dictp['ULYMAP']=dictecosg['north']
    dictp['XDIM']=str(abs((float(dictecosg['west'])-float(dictecosg['east']))/float(int(dictecosg['cols']))))
    dictp['YDIM']=str(abs((float(dictecosg['north'])-float(dictecosg['south']))/float(int(dictecosg['rows']))))
    with open(outfn,'w+') as f:
        for i,v in dictp.items():
            f.write(i+' '+v+'\n')
        f.close()
    return(dictp,dictecosg)


# ## Create a soft link to ECOSG .dir file and create a python readable .hdr file

# In[6]:


def Create_readable_ecosg_cover(input_ecosgcoverpath):
    fnlink=os.path.join(os.getcwd(),input_ecosgcoverpath.split('/')[-1])
    if not os.path.exists(fnlink):
        os.symlink(input_ecosgcoverpath,os.path.join(os.getcwd(),input_ecosgcoverpath.split('/')[-1]))
    fnhdr=input_ecosgcoverpath.replace('.dir','.hdr')    
    outfn=fnlink.replace('.dir','.hdr') 
    (dictp,dictecosg)=create_readablehdr(fnhdr,outfn)
    return(dictp,dictecosg,outfn,fnlink)
    


# ## These functions replace make new val0.F90

# ### Seacrh the bounding points corresponding to lai/albedo in the ECOSG cover file

# In[7]:


def Search_bounding_index(dictlai,srccover):
    wcover=rasterio.windows.from_bounds(float(dictlai['west']),
                                        float(dictlai['south']),
                                        float(dictlai['east']),
                                        float(dictlai['north']),
                                        transform=srccover.transform).round_offsets().round_shape()
    GCD=np.gcd(wcover.width,wcover.height)
    return(wcover,GCD)


# ### Creates applies ECOSG cover mask and values change to a portion of the file

# In[8]:


def Writewindow_tempfile(wcover,windex,hindex,GCD,
                         srccover,srclai):
        Wlai=Window(windex, hindex, GCD, GCD)
        Weco=Window(windex+wcover.col_off, hindex+wcover.row_off, GCD, GCD)
        ecoval = srccover.read(1, window=Weco).astype(np.int16)
        laival= srclai.read(1, window=Wlai).astype(np.int16)
        if (np.max(laival)>127 or np.min(laival<=-1)):
            print ("problem, val < -1!")
        #laival2[ecoval<4]=1
        #laival2[laival<=-1]=1
        laival2=((ecoval*100+laival)*((ecoval>=4)*(laival>=0))+1*(ecoval<4)+1*(laival<=-1)).astype(np.int16)
        trans=rasterio.windows.transform(Wlai,srclai.transform)
        tempfile='temp'+str(hindex)+str(windex)+'.tif'
        src=rasterio.open(tempfile,
                          mode='w',
                          Driver='gTiff',
                          width=GCD,
                          height=GCD,
                          count=srclai.count,
                          dtype=rasterio.uint16,
                          crs=srclai.crs,
                          transform=trans,
                          nodata=0)
        src.write(laival2,1)
        src.close()
        return(tempfile)
        


# ### Make new value all together

# In[9]:


def make_new_val0(windowc,GCD,srccover,srclai,mosaicfile='test.dir'):
    src_files_to_mosaic=[]
    tempfilelist=[]
    for h in range(0,windowc.height,GCD):
        for w in range(0,windowc.width,GCD):
            tempfile=Writewindow_tempfile(windowc,w,h,GCD,srccover,srclai)
            src_files_to_mosaic.append(rasterio.open(tempfile))
            tempfilelist.append(tempfile)
    mosaic, out_trans = merge(src_files_to_mosaic)
    out_meta=srclai.meta.copy()
    out_meta.update({"height": mosaic.shape[1],
                     "width": mosaic.shape[2],
                     "transform": out_trans,
                     "dtype": rasterio.uint16
                    })
    with rasterio.open(mosaicfile, "w", **out_meta) as dest:
        dest.write(mosaic)
    dest.close()
    [os.remove(f) for f in tempfilelist]
    return(out_meta,out_trans)


# ## Runs make new value and the compression script together

# In[10]:


def Compressfile(output_ending,fn,fncoverlink):
    # Creates the output directory
    outpath=Createoutputdir(output_ending)
    # Creates output file path
    outdirfn=os.path.join(outpath,fn.split('/')[-1])
    # Creates the hdr python readble file and extact info
    (dictlaialb,dictuncompress)=create_ecosghdr(fn.replace('.dir','.hdr'),outdirfn.replace('.dir','.hdr'))
    # Read and rewrite the Namelist to 
    NAMELISTpath=os.path.join(os.getcwd(),'Namelist.nml')
    nml = f90nml.read(NAMELISTpath)
    nml['inputs']['infile']=fn
    nml['inputs']['compressed_file']=outdirfn
    nml['inputs']['ncol'] = int(int(dictuncompress['ncols'])/3)
    nml['inputs']['nlin'] = int(int(dictuncompress['nrows'])/3)
    os.remove(NAMELISTpath) # remove before rewritting the file
    nml.write(NAMELISTpath)
    srccoverSG=rasterio.open(fncoverlink)
    srclaialb=rasterio.open(fn)
    (windowc,GCD)=Search_bounding_index(dictlaialb,srccoverSG)
    om,ot=make_new_val0(windowc,GCD,srccoverSG,srclaialb,mosaicfile=nml['inputs']['outfile'])
    compressscipt=os.path.join(os.getcwd(),'Compress_val.F90')
    cmd2='gfortran -o compiling.out ' +compressscipt
    subprocess.run(cmd2,shell=True)
    subprocess.run('./compiling.out',shell=True)
    #os.remove(os.path.join(os.getcwd(),nml['inputs']['outfile']))
    
    return(outdirfn)


# # MAIN

# In[11]:


# list files to uncompress
listdirfiles=[os.path.join(input_directory,f) for f in np.sort(os.listdir(input_directory)) if f.endswith('.dir')]
# Get the cover data
(dictp,dictecosg,outfn,fncoverlink)=Create_readable_ecosg_cover(input_ecosgcoverpath)
# Compress files and return the location
Uncompressedlist=[Compressfile(output_end,fn,fncoverlink) for fn in listdirfiles]


# In[ ]:




