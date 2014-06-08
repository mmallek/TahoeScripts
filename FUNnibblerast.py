import arcpy

from arcpy import env
from arcpy.sa import *

# CoverRaster_0515_1541
def nibblerast(inlayer, outlayer, corebuffer="", cover=False, aspen=False, fillbuffer=False, condition=False, aspencond=False, condition88=False):
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
        arcpy.gp.Con_sa(corebuffer, out2, outlayer, "", "\"Value\" = 1")

    if condition:
        outCon1 = Con(corebuffer > 0, Nibble(Con(IsNull(inlayer)==1, 99, inlayer), inlayer, "DATA_ONLY"))
        outCon1.save(outlayer)

    if aspencond:
        outCon1 = Con(corebuffer > 0, Con(IsNull(inRaster)==1, 99, Times(inRaster, 10)))
        outCon1.save(outlayer)

    if condition88:
        condlayer = inlayer
        outCon1 = Con(condlayer, condlayer, Nibble(condlayer, SetNull(condlayer, condlayer, 
                    "VALUE NOT IN (10,20,21,22,30,31,32)")), "VALUE IN (0,40,41,42,43)")
        outCon1.save(outlayer)

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
    arcpy.gp.Con_sa("FullProjectArea_0528_1920", inlayer, outlayer, "", "\"Value\" =1")
    
    # Nibble YPN out of core
    outCon3 = Con(outCon2,outCon2, Nibble(outCon2, SetNull(outCon2,outCon2, 
        "VALUE NOT IN (1,2,3,6,14,15,16,17,19,20,21)")), "VALUE IN (5,7,9,10,11,12,13)")
    
    # Use Con to put the cover layer back together in Raster Calculator
    outCon4 = Con(proj == 1, outCon3, outCon1)
    #arcpy.gp.Con_sa("FullProjectArea_0528_1920", inlayer, outlayer, "", "\"Value\" =1")


    # Save the outputs 
    outCon4.save(outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")


# Combine
def combine_rast(outlayer, *inrasters):
    """ Combine multiple rasters to add information to a base raster such as cover or condition 
    "CoverRaster3_0529_1547"
    "Aspen_0522_1717"
    "Meadow999_0522_1720"
    "Streams999_0521_2328"
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    
    # Execute Combine

    arcpy.gp.Combine_sa(inrasters, outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")
    

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
    landcover = "CoverRaster3_052"
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

    arcpy.AddField_management(inlayer, "YHR", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

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

def condcombine(aspencondlayer, condlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    # Process: Con
    arcpy.gp.Con_sa(aspencondlayer, aspencondlayer, outlayer, condlayer, """ "Value" < 60 """)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")

def killbackground(inlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    arcpy.gp.Con_sa("FullProjectArea_0528_1920", inlayer, outlayer, "", "\"Value\" =1")
    
    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")


def assigncondition(inlayer):
    """
First combine step uses aspen, meadow, stream, and cover rasters.
This function creates a new field for condition types and assigns each
row in the combined attribute table to a new condition value.
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # add new field to store results (may need to comment out if redoing code below)
    arcpy.AddField_management(inlayer, "Condition", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    #  0   Non-seral
    #  10  EARLY_ALL
    #  20  MID_CL
    #  21  MID_MOD
    #  22  MID_OP
    #  30  LATE_CL
    #  31  LATE_MOD
    #  32  LATE_OP
    #  40  EARLY_ASP
    #  41  MID_ASP
    #  42  MID_AC
    #  43  LATE_CA

    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        if row.getValue("Value") == 1:
            row.Condition = 30 # late closed
        if row.getValue("Value") == 2:
            row.Condition = 20 # mid closed
        if row.getValue("Value") == 3:
            row.Condition = 31 # late mod
        if row.getValue("Value") == 4:
            row.Condition = 21 # mid mod
        if row.getValue("Value") == 5:
            row.Condition = 10 # early all
        if row.getValue("Value") == 6:
            row.Condition = 22 # mid open  
        if row.getValue("Value") == 7:
            row.Condition = 0 # static
        if row.getValue("Value") == 8:
            row.Condition = 32 # late open
        if row.getValue("Value") == 10:
            row.Condition = 42 # mid aspen conifer
        if row.getValue("Value") == 20:
            row.Condition = 30 # late closed
        if row.getValue("Value") == 30:
            row.Condition = 43 # late conifer aspen
        if row.getValue("Value") == 40:
            row.Condition = 41 # mid aspen
        if row.getValue("Value") == 50:
            row.Condition = 40 # early asp
        rows.updateRow(row)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

def parse_covcond(inlayer, covfield, condfield):
    """
    Use after combining cover and condition rasters.
    if statements should be written to eliminate errors (e.g. reassign AGR to static)
    if no determination could be made, cells are assigned to 99 and nibbled
    Have to check for field names after running combine
    covfield = "CoverRaster8_052"
    condfield = "ConditionSorted_"
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # add new field to store results (may need to comment out if redoing code below)
    arcpy.AddField_management(inlayer, "Condition", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        # if a static type, then code as static
        if row.getValue(covfield) in (1,2,4,8,27,28):
            row.Condition = 0
        # if early and a type that can be early, code as early
        elif row.getValue(condfield) == 10 and row.getValue(covfield) in (3,5,7,9,10,11,12,13,14,15,17,18,19,20,21,24,25,26,29,30):
            row.Condition = 10 # early all
        # if an aspen type and condition, use condition value
        elif row.getValue(covfield) in (6,16,22,23,31) and row.getValue(condfield) in (30,40,41,42,43):
            row.Condition = row.getValue(condfield)
        # if a type that fills the full range of mid and late, use condition value
        elif row.getValue(covfield) in (5,9,10,11,13,14,15,17,18,19,21,24,25,26,29,30) and row.getValue(condfield) in (20,21,22,30,31,32):
            row.Condition = row.getValue(condfield)
        # shrubs (CMM, LSG, SAGE) can be early all, mid moderate, and late closed only
        elif row.getValue(covfield) in (3,7,20) and row.getValue(condfield) in (21,30):
            row.Condition = row.getValue(condfield)
        # shrubs with the wrong condition 
        elif row.getValue(covfield) in (3,7,20) and row.getValue(condfield) in (20,22):
            row.Condition = 21
        elif row.getValue(covfield) in (3,7,20) and row.getValue(condfield) in (31,32):
            row.Condition = 30
        # MRIP is only open
        elif row.getValue(covfield) == 12 and row.getValue(condfield) in (22,32):
            row.Condition = row.getValue(condfield)
        elif row.getValue(covfield) == 12 and row.getValue(condfield) in (20,21):
            row.Condition = 22
        elif row.getValue(covfield) == 12 and row.getValue(condfield) in (30,31):
            row.Condition = 32
        else:
            row.Condition = 88
        rows.updateRow(row)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows


