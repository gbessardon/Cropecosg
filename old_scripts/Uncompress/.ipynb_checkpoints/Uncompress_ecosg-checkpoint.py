#!/usr/bin/env python
# coding: utf-8

# # Declare libraries

# In[1]:


import os
import f90nml
import pandas as pd
import subprocess


# In[2]:


input_directory='/data/gbessardon/ECOCLIMAP-SG/LAI/300M'
output_end='Uncompressed_LAI' 


# # Decalare functions

# ## Function to Create the outputdir

# In[3]:


def Createoutputdir(outputdir):
    outpath=os.path.join(os.getcwd(),outputdir)
    if not os.path.isdir(outpath):
        os.mkdir(outpath)
    return outpath


# ## Re-write ECOSG .hdr file into python readable hdr file

# In[4]:


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


# In[5]:


def Uncompressfile(output_ending,fn):
    # Creates the output directory
    outpath=Createoutputdir(output_ending)
    # Creates output file path
    outdirfn=os.path.join(outpath,fn.split('/')[-1])
    # Creates the hdr python readble file and extact info
    (dictp,dictecosg)=create_readablehdr(fn.replace('.dir','.hdr'),outdirfn.replace('.dir','.hdr'))
    # Read and rewrite the Namelist to 
    NAMELISTpath=os.path.join(os.getcwd(),'namelist.nml')
    nml = f90nml.read(NAMELISTpath)
    nml['inputs']['infile']=fn
    nml['inputs']['outfile']=outdirfn
    nml['inputs']['ncol'] = int(int(dictecosg['cols'])/3)
    nml['inputs']['nlin'] = int(int(dictecosg['rows'])/3)
    os.remove(NAMELISTpath) # remove before rewritting the file
    nml.write(NAMELISTpath)
    Uncompressscipt=os.path.join(os.getcwd(),'Uncompressecosg.F90')
    cmd2='gfortran -o compiling.out ' +Uncompressscipt
    subprocess.run(cmd2,shell=True)
    subprocess.run('./compiling.out',shell=True)
    return(outdirfn)


# # Main bit

# In[6]:


# list files to uncompress
listdirfiles=[os.path.join(input_directory,f) for f in os.listdir(input_directory) if f.endswith('.dir')]
# Uncompress files and return the location
Uncompressedlist=[Uncompressfile(output_end,fn) for fn in listdirfiles]

