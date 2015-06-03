import arcpy
from arcpy import env
from arcpy.sa import *

def EVegpolytoraster(EVeg, field, out):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    
    # Set local variables
    inPath = "Y:/Tahoe/GISdata/Scratch3.gdb/"
    inFeatures = inPath + EVeg
    valField = field
    outPath = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    outRaster = outPath + out
    assignmentType = "MAXIMUM_AREA"
    #priorityField = "MALES"
    cellSize = 30

    
    # Execute PolygonToRaster
    arcpy.PolygonToRaster_conversion(in_features=inFeatures, value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)
    
    
    # want to run on
    # AYHR10: "CoverPoly2Raster_051414"
    # ConditionClass2: "CondPoly2Raster_051414" 

# AGR = 11
# BAR = 7
# CMM = 20
# EARLY = 4
# GRASS = 5
# LPN = 15
# LSG = 21
# MED = 13
# MEG = 1
# MRIP = 12
# OAK = 6
# OCFW = 2
# RFR = 14
# ROAD = 8
# SAGE = 19
# SCN = 16
# SMC = 3
# URB = 10
# WAT = 9
# WWP = 17
# YPN = 18

def genpolytoraster(inlayer, field, outlayer, stream=False, um=False, chap=False, fire=False, 
    flowlinePA=False, flowlineIE=False, flowlineI=False, road=False):
    """ 
    inlayer and outlayer need to be in quotes
    EVeg full path is: Y:\Tahoe\GISdata\Scratch3.gdb\EVeg_070813_1545
    Field for streams should be StrOrder_Grp
    Field for Aspen as cover (to get one type) is NRCS_PLANT_CODE 
    Field for Meadow (to get one type) is STATE
    Geology layer should be preprocessed to match extent of project area
    Field for Geology100k ["Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/Geology_0523_1817"] is ORIG_LABEL
    Field for Condition is ConditionClass2
    Field for Aspen as condition is "CondClass"
    Field for Chaparral is "AYHR10"
    """
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    
    # Set local variables
    valField = field
    outPath = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    outRaster = outPath + outlayer
    assignmentType = "MAXIMUM_AREA"
    cellSize = 30

    if stream:
        assignmentType = "MAXIMUM_LENGTH"
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule1 = """ "StrOrder_Grp" = '1-large' """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule1)
        arcpy.PolylineToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if um: # background processing may need to be turned off for this to work
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule2 = """ "ORIG_LABEL" = 'um' """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule2)
        print "selection complete"
        arcpy.PolygonToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        print "conversion complete"
        arcpy.Delete_management("featurelayer")
        print "deleted featurelayer"

    if chap:
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule = """ "REGIONAL_DOMINANCE_TYPE_1" In ('BX','CL','CQ','CS','CW','KP','MN','CH','CI','CG','CM','CP','CV','CX','CY') """ 
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule)
        arcpy.PolygonToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if fire:
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")        
        rule = """ "BURNSEV" = 4 AND "BEST_ASSESS" = 'YES' """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule)
        arcpy.PolygonToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if flowlinePA:
        assignmentType = "MAXIMUM_LENGTH"
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule1 = """ "FCode" In (46006, 55800) """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule1)
        arcpy.PolylineToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if flowlineIE:
        assignmentType = "MAXIMUM_LENGTH"
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule1 = """ "FCode" In (46003,46007) """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule1)
        arcpy.PolylineToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if flowlineI:
        assignmentType = "MAXIMUM_LENGTH"
        arcpy.MakeFeatureLayer_management(inlayer, "featurelayer")
        rule1 = """ "FCode" = 46003 """
        arcpy.SelectLayerByAttribute_management("featurelayer", "NEW_SELECTION", rule1)
        arcpy.PolylineToRaster_conversion(in_features="featurelayer", value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        arcpy.Delete_management("featurelayer")

    if road:
        # use field 'CREATED_BY'
        assignmentType = "MAXIMUM_LENGTH"
        arcpy.PolylineToRaster_conversion(in_features=inlayer, value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)    
        #arcpy.Delete_management("featurelayer")

    else:
        arcpy.PolygonToRaster_conversion(in_features=inlayer, value_field=valField, out_rasterdataset=outRaster, cell_assignment=assignmentType, cellsize=cellSize)


def background(inlayer, outlayer):
    """This function converts rasters where the background is "no data" to a raster where the background
    is 99999 within the project boundary and "no data" elsewhere. Use it on Aspen, Streams, Meadows, and Cover Layer"""

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # convert background
    outCon = Con(IsNull(inlayer) == 1, 99999, inlayer)
    outCon.save(outlayer)


    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")



#FUNpolytoraster.genpolytoraster(inlayer="Y:/Tahoe/GISdata/TNF_RMLands_4.gdb/Streams", field="StrOrder_Grp", outlayer="Streams_0521_2317", stream=True)
#FUNpolytoraster.genpolytoraster(inlayer="Y:/Tahoe/GISdata/TNF_RMLands_17_1.gdb/RML_Aspen", field="NRCS_PLANT_CODE", outlayer="Aspen_0522_1628")
#FUNpolytoraster.genpolytoraster(inlayer="Y:/Tahoe/GISdata/TNF_RMLands_17_1.gdb/RML_Meadow", field="STATE", outlayer="Meadow_0522_1631")

#FUNpolytoraster.genpolytoraster("Y:/Tahoe/GISdata/Geology/CAgeol_lcc/cageol_poly_lcc.shp", "ORIG_LABEL", "Ultramafic_0523_1805", um=True)







