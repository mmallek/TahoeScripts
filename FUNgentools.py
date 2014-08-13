# FUNgentools

import arcpy
from arcpy import env
from arcpy.sa import *



def myslope(inlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Execute Slope
    outSlope = arcpy.gp.Slope_sa(inlayer, "slopeint", "PERCENT_RISE", "1")
    
    # Execute Con
    arcpy.gp.Con_sa("slopeint", "126", "conslopeint", "slopeint", "Value > 126")

    arcpy.gp.Con_sa("FullProjectArea_0528_1920", "conslopeint", outlayer, "", "\"Value\" =1")

    arcpy.Delete_management("slopeint")
    arcpy.Delete_management("conslopeint")

    # Save the output 
    #outSlope.save(outlayer)

def clippolys(inlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Execute Clip 
    arcpy.Clip_analysis(inlayer, "Y:/Tahoe/GISdata/TNF_RMLands_1.gdb/ProjectBoundary_10kbuff", outlayer)

def killbuffer(inlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    arcpy.env.mask = mask_source

    out = inlayer
    out.save(outlayer)

    