# Functions to reclassify RMLands output

import numpy as np
from glob import glob
#from PIL import Image
#import gdal
from osgeo import gdal
from gdalconst import *
import arcpy
from arcpy import env
from arcpy.sa import *

def reclass(x, table):
    """ Take a 2d array (originally from an image), and replace each value from
        the first column of the conversion table with the corresponding value
        in the second column"""
    assert x.ndim == 2, "Must input 2d array"
    orig_class = table[:, 0]
    new_class = table[:, 1]

    # each layer says which pixels match the orig_class:
    mask = x[..., None] == orig_class

    # multiply to convert True's to new_class
    new = mask * new_class

    # then sum along the third axis (axis=2) to flatten it.
    new = np.sum(new, axis=2)

    return new

def reclass_files(session, table_file, suffix, file_pattern='run001\\ts_grp*\\covcond*', path='Z:\\Working\\MaritzaZW\\results\\', mac=False):
    """ loops through and opens all image files that match the file_pattern
        and it converts them according to the table stored at table_file """
    # may need extra options:
    conversion_table = np.genfromtxt(table_file, delimiter=',', skiprows=1, dtype='uint16') #skip_header=1)

    format = "GTiff"
    driver = gdal.GetDriverByName( format )

    # mac
    if mac:
        file_names = glob(path+'session'+session+'/'+file_pattern)
    # windows
    else:
        file_names = glob(path+'session'+session+'\\'+file_pattern)

    print 'hi'
    for file_name in file_names:
        print 'starting loop'
        # orig = np.asarray(Image.open(file_name))
        orig = gdal.Open(file_name, GA_ReadOnly)
        orig_array = orig.ReadAsArray()
        print 'about to run reclass'
        new = reclass(orig_array, conversion_table)

        # i = Image.fromarray(new)
        # i.save(file_name.replace('.tif', '_'+suffix+'.tif'))
        print 'halfway done'
        # we already have the raster with exact parameters that we need
        # so we use CreateCopy() method instead of Create() to save our time
        outData = driver.CreateCopy(file_name.replace('.tif', '_'+suffix+'.tif'), orig, 0)
        outData.GetRasterBand(1).WriteArray(new)
        print 'saving ', file_name

#### Example usage:

# import reclass

# reclass.reclass_files('folder*/images*.tif', 'my_cc_table.txt')
# FUN_rmlreclass.reclass_files('001', '/Users/mmallek/Tahoe/RMLands/csvs/reclass3.csv',
#     'r3', 'run001/ts_grp*/covcond*', path='/Users/mmallek/Tahoe/GISdata/')

#############################################

def clip(session, file_pattern='run001\\ts_grp*\\covcond*', path='Z:\\Working\\MaritzaZW\\results\\', mac=False):
    '''Clipping happens after rescaling, which is also to say that rescaling has to be done
    on the unclipped output file. Both the unrescaled and rescaled grids can be analyzed in
    Fragstats. This function's specific purpose is to clip RMLands output grids/tiffs to the
    extent of the project area, exclusive of the 10 km boundary.
    '''

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    # set environment settings
    env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    #env.workspace = "Y:/Tahoe/GISdata/" + workspace

    # use a gdb for a scratch workspace to ensure saving is correct
    # http://gis.stackexchange.com/questions/58816/what-causes-runtimeerror-error-010240-saving-after-cellstatistics
    env.scratchWorkspace = "Y:/Tahoe/GISdata/ScratchWorkspace.gdb"

    # mac
    if mac:
        file_names = glob(path+'session'+session+'/'+file_pattern)
    # windows
    else:
        file_names = glob(path+'session'+session+'\\'+file_pattern)

    for file_name in file_names:
        # Execute Con using a map algebra expression instead of a where clause
        outCon = Con(Raster("Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/CoreBuffer_0516_1938") == 1, Raster(file_name))
        outCon.save(file_name.replace('.tif', '_clip.tif'))
        #arcpy.Delete_management("outCon")

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")

    # There will be a lock file but it *should* go away when you quit ArcMap.

# Y:\Tahoe\GISdata\session001

#def sad_reclass():


from numpy import copy

np.copy(theArray)
for fromval, toval in conversion_table.iteritems(): newArray[theArray==k] = v




