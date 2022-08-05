import gdal
import os
import rasterio
from rasterio.windows import Window
import numpy as np



def Createoutputdir(outputdir):
    outpath=os.path.join(os.getcwd(),outputdir)
    if not os.path.isdir(outpath):
        os.mkdir(outpath)
    return outpath


def Crop_data(inputfn,outputdir,xmin, ymin, xmax, ymax):
    outpath=Createoutputdir(outputdir)
    opt=gdal.WarpOptions(format='EHdr',outputBounds=(xmin, ymin, xmax, ymax))
    outputfn=os.path.join(outpath,inputfn.split('/')[-1])
    gdal.Warp(outputfn,inputfn,options=opt)
    return

def Crop_data_rasterio(inputfn,outputdir,xmin, ymin, xmax, ymax):
    outpath=Createoutputdir(outputdir)
    src=rasterio.open(inputfn)
    Window=src.window(xmin, ymin, xmax, ymax).round_offsets().round_lengths()
    cropdata=src.read(1,window=Window)
    trans=rasterio.windows.transform(Window,src.transform)
    tempfile='test.tif'
    srcout=rasterio.open(tempfile,
                        mode='w',
                        Driver='gTiff',
                        width=Window.width,
                        height=Window.height,
                        count=src.count,
                        dtype=np.int16,
                        crs=src.crs,
                        transform=trans,
                        nodata=src.nodata,
                        compress='lzw')
    srcout.write(cropdata,1)
    srcout.close()
    outputfn=os.path.join(outpath,inputfn.split('/')[-1])
    gdal.Translate(outputfn,'test.tif',format=src.driver)
    src.close()
    os.remove('test.tif')
    return