# GDAL web tutorial helpful hints and tips

# load a dataset
import gdal
from gdalconst import *
dataset = gdal.Open( filename, GA_ReadOnly )

# Getting Dataset Information
# driver, raster size, projection, origin, cell size

print 'Driver: ', dataset.GetDriver().ShortName,'/', \
      dataset.GetDriver().LongName
print 'Size is ',dataset.RasterXSize,'x',dataset.RasterYSize, \
      'x',dataset.RasterCount
print 'Projection is ',dataset.GetProjection()

geotransform = dataset.GetGeoTransform() # this is the function that pulls these details
if not geotransform is None:
    print 'Origin = (',geotransform[0], ',',geotransform[3],')'
    print 'Pixel Size = (',geotransform[1], ',',geotransform[5],')'

# Fetching a Raster Band
# At this time access to raster data via GDAL is done one band at a time. 
# Also, there is metadata, blocksizes, color tables, and various other information 
# available on a band by band basis. The following codes fetches a GDALRasterBand 
# object from the dataset (numbered 1 through GetRasterCount()) and displays 
# a little information about it.

band = dataset.GetRasterBan

print 'Band Type=',gdal.GetDataTypeName(band.DataType)

min = band.GetMinimum()
max = band.GetMaximum()
if min is None or max is None:
    (min,max) = band.ComputeRasterMinMax(1)
print 'Min=%.3f, Max=%.3f' % (min, max)

if band.GetOverviewCount() > 0:
    print 'Band has ', band.GetOverviewCount(), ' overviews.'

if not band.GetRasterColorTable() is None:
    print 'Band has a color table with ', \
    band.GetRasterColorTable().GetCount(), ' entries.'

# Reading Raster Data
# There are a few ways to read raster data, but the most common is via the 
# GDALRasterBand::RasterIO() method. This method will automatically take care 
# of data type conversion, up/down sampling and windowing. The following code 
# will read the first scanline of data into a similarly sized buffer, converting 
# it to floating point as part of the operation.

scanline = band.ReadRaster( 0, 0, band.XSize, 1, \
                             band.XSize, 1, GDT_Float32 )

# Note that the returned scanline is of type string, and contains xsize*4 bytes 
# of raw binary floating point data. This can be converted to Python values 
# using the struct module from the standard library:

import struct

tuple_of_floats = struct.unpack('f' * b2.XSize, scanline)

# Closing the Dataset
# Call GDALClose() 
# or?
# Once we're done, close properly the dataset
dst_ds = None

# Techniques for Creating Files

format = "GTiff"
driver = gdal.GetDriverByName( format )
metadata = driver.GetMetadata()
if metadata.has_key(gdal.DCAP_CREATE) \
   and metadata[gdal.DCAP_CREATE] == 'YES':
    print 'Driver %s supports Create() method.' % format
if metadata.has_key(gdal.DCAP_CREATECOPY) \
   and metadata[gdal.DCAP_CREATECOPY] == 'YES':
    print 'Driver %s supports CreateCopy() method.' % format

# Using Create()

dst_ds = driver.Create( dst_filename, 512, 512, 1, gdal.GDT_Byte )
    
import osr
import numpy

dst_ds.SetGeoTransform( [ 444720, 30, 0, 3751320, 0, -30 ] )
    
srs = osr.SpatialReference()
srs.SetUTM( 11, 1 )
srs.SetWellKnownGeogCS( 'NAD27' )
dst_ds.SetProjection( srs.ExportToWkt() )

raster = numpy.zeros( (512, 512), dtype=numpy.uint8 )    
dst_ds.GetRasterBand(1).WriteArray( raster )

# Once we're done, close properly the dataset
dst_ds = None

############################################################################################
############################################################################################
############################################################################################

# from http://gis-lab.info/qa/gdal-python.html
gdalData = gdal. Open (  "/home/alex/test/input.tiff"  ) 
# raster size 
xsize = gdalData. RasterXSize 
ysize = gdalData. RasterYSize 
# get an array of raster 
Raster = gdalData. ReadAsArray ( ) 
# iterate through all the pixels in the raster 
for col in  range ( xsize ) :
   for row in  range ( ysize ) :
     # if the pixel value is 5, then change it to 10 
    # Otherwise, the value remains unchanged 
    if Raster [ row , col ]  ==  5 :
      Raster [ row , col ]  =  10
