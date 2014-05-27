import arcpy

from arcpy import env
from arcpy.sa import *

# CoverRaster_0515_1541
def nibblerast(inlayer, outlayer, corebuffer="", cover=False, aspen=False, fillbuffer=False):
    """
    if cover: for nibbling away cover types
    if nibble: for cleaning up the aspen pixels.
        Thus aspen types are allowed to nibble ("value not in") the unclassified aspen types (absent)
    if fillbuffer: for expanding into buffer gaps from EVeg. inlayer is the reclassified layer from previous step
    remember: 'value not in' nibbles, 'value in' is static, and missing values are nibbled away
    """
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Set local variables
    #inlayer = #"CoverRaster_0515_1541"
    inRaster = inlayer
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")
    
# AGR = 11      # MEG = 1 
# BAR = 7       # OCFW = 2 
# CMM = 20      # SMC = 3 
# EARLY = 4     # EARLY = 4 
# GRASS = 5     # GRASS = 5
# LPN = 15      # OAK = 6
# LSG = 21      # BAR = 7
# MED = 13      # ROAD = 8
# MEG = 1       # WAT = 9
# MRIP = 12     # URB = 10
# OAK = 6       # AGR = 11
# OCFW = 2      # MRIP = 12
# RFR = 14      # MED = 13
# ROAD = 8      # RFR = 14
# SAGE = 19     # LPN = 15
# SCN = 16      # SCN = 16 
# SMC = 3       # WWP = 17      
# URB = 10      # YPN = 18 
# WAT = 9       # SAGE = 19 
# WWP = 17      # CMM = 20
# YPN = 18      # LSG = 21
    if cover:
        outCon1 = Con(inRaster,inRaster, 
            Nibble(inRaster, 
                SetNull(inRaster,inRaster, "AYHR10 NOT IN ('CMM','LPN','LSG','MEG','OAK','OCFW','RFR','SAGE','SCN','SMC','WWP','YPN')")), 
                    "AYHR10 IN ('AGR','BAR','GRASS','MED','MRIP','URB','WAT')")
        # Save the outputs 
        outCon1.save(outlayer)
    
    if aspen:
        outCon1 = Con(inRaster,inRaster, 
            Nibble(inRaster, 
                SetNull(inRaster,inRaster, "VALUE NOT IN (30,31,32,33,34)")), 
                    "VALUE IN (1,2,3,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,99999)")
        # Save the outputs 
        outCon1.save(outlayer)

    if fillbuffer:
        #out1 = Con(IsNull(cover) == 1, 99999, cover)
        out = Con(corebuffer > 0, Con(IsNull(inRaster) == 1, 99999, inRaster))
        out2 = Con(out,out, 
                    Nibble(out, 
                        SetNull(out,out, "VALUE NOT IN (1,2,3,6,14,15,16,17,19,20,21)")), 
                            "VALUE IN (5,7,9,10,11,12,13,18,30,31,32,33,34)") 
        arcpy.gp.Con_sa(corebuffer, out2, outlayer, "", "\"Value\" > 0")
    

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")


def removeYPN(inlayer, outlayer):
    # inlayer = "Nibble1_0515_1725"
    # outlayer = "RemoveYPN_0517_2130"
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    
    # Clip landcover to core area
    outCon1 = inlayer
    proj = Raster("Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/CoreBuffer_0516_1938")
    outCon2 = Con(proj == 1, outCon1) 
    
    # Nibble YPN out of core
    outCon3 = Con(outCon2,outCon2, Nibble(outCon2, SetNull(outCon2,outCon2, 
        "VALUE NOT IN (1,2,3,6,14,15,16,17,19,20,21)")), "VALUE IN (5,7,9,10,11,12,13)")
    
    # Use Con to put the cover layer back together in Raster Calculator
    outCon4 = Con(proj == 1, outCon3, outCon1)
    
    # Save the outputs 
    outCon4.save(outlayer)
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")
    # Clip Landcover to Core area in Raster Calculator 
    # Con("CoreBuffer_0822_1148" == 1, "Nibble410_1608") 
    # Nibble YPN (value=19) in Raster Calculator 
    #Con("CoverCore_1713","CoverCore_1713", Nibble("CoverCore_1713", SetNull("CoverCore_1713","CoverCore_1713", "VALUE NOT IN (1,2,3,6,7,15,16,17,18,20,21,22)")), "VALUE IN (5,8,9,11,12,13,14)") \end{lstlisting}
    #Use Con to put the cover layer back together in Raster Calculator 
    #Con("CoreBuffer_0822_1148" == 1, "NibbleYPN_1717", "Nibble410_1608")

# Combine
def combine_rast(outlayer, *inrasters):
    """ Combine multiple rasters to add information to a base raster such as cover or condition 
    "Cover_0521_2119"
    "Aspen_0522_1628"
    "Meadow_0522_1631"
    "Streams_0521_2328"
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WovkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    
    # Execute Combine

    arcpy.gp.Combine_sa(inrasters, outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")
    
  


 # FUNnibblerast.combine_rast("Combine_0522_1722","Cover_0521_2119","Aspen_0522_1717","Meadow999_0522_1720","Streams999_0521_2328")


def parse_combine(inlayer):
    """
First combine step uses aspen, meadow, stream, and cover rasters.
This function creates a new field for cover types and assigns each
row in the combined attribute table to a new cover value.
    """

# LPN = 15
# MED = 13
# RFR = 14
# SCN = 16
# SMC = 3
# WAT = 9
# YPN = 18

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # may decide to change these, but since I don't really anticipate reusing this function...
    landcover = "Cover_0521_2119"
    aspen = "Aspen_0522_1717"
    meadow = "Meadow999_0522_1"
    streams = "Streams999_0521_"

    # add new field to store results (may need to comment out if redoing code below)
    #arcpy.AddField_management(in_table=inlayer, field_name="Cover", field_type="LONG")
    arcpy.AddField_management(inlayer, "Cover", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")


    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        #first priority is streams
        if row.getValue(streams) == 1:
            row.Cover = 9
        elif row.getValue(aspen) == 1 and row.getValue("Cover") is None:
            # Aspen cover types
            # 30 SMC_ASP
            # 31 RFR_ASP
            # 32 LPN_ASP
            # 33 SCN_ASP
            # 34 YPN_ASP
            # 88 ASP + some other type
            if row.getValue(landcover) == 3:
                row.Cover = 30
            elif row.getValue(landcover) == 14:
                row.Cover = 31
            elif row.getValue(landcover) == 15:
                row.Cover = 32
            elif row.getValue(landcover) == 16:
                row.Cover = 33
            elif row.getValue(landcover) == 18:
                row.Cover = 34
            else:
                row.Cover = 88
        elif row.getValue(meadow) == 1 and row.getValue("Cover") is None:
            row.Cover = 13
        elif row.getValue("Cover") is None:
            row.Cover = row.getValue(landcover)
        rows.updateRow(row)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

def lookitup(inlayer, field, outlayer):
    """
    Cover will be the field most of the time
    """

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    # execute lookup
    out = Lookup(inlayer, lookup_field=field)

    # save output
    out.save(outlayer)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")



def seqvalue(inlayer, field1, direction):
    rows = arcpy.UpdateCursor(inlayer, "", "", "", " ".join([field1, direction]))
    print "update cursor created"
    i = 1
    for row in rows:
        row.YHR = i
        i += 1
        rows.updateRow(row)
    print "updated rows"

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

