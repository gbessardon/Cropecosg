import gdal
import os



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