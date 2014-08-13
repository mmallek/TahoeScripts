#FUNCTIONs for Age Processing

# after you have a point layer that has all available age data as for the same year
# in this case, 2010, the following functions can be applied

import arcpy
from arcpy import env
#from arcpy.sa import Idw
#from arcpy.sa import Con 
#from arcpy.sa import Int
from arcpy.sa import *

def ageprocess(inlayer, agefield, corebuffer, outlayer):
# agefield if starting from preprocessed layer is "Age_2010_Merge"
# corebuffer is probably 

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    #default params for IDW
    cell_size = 30
    power = 2
    search_radius = "RadiusVariable(12)"

    # run IDW
    #outIdw = arcpy.gp.Idw_sa(inlayer, agefield, "", "30", "2", "VARIABLE 12", "")
    outIdw = Idw(inlayer, agefield, cell_size, power, search_radius, "")
    #outIdw.save("temp")
    print "IDW created"

    
    # round ages and clip to project buffer
    #temp = outIdw + 0.5
    #temp = Int(temp)
    #temp = temp / 10
    #temp = Int(temp)
    #temp = temp * 10

    #outCon = Con(corebuffer == 1, temp)

    # for 5-year timestep
    outRaster = Int(Int(outIdw + 2.5) / 5 ) * 5
    # for 10-year timestep: Int(Int(outIdw + 0.5) / 10) * 10)
    print "raster calculations complete"

    arcpy.gp.Con_sa(corebuffer, outRaster, outlayer, "", "\"Value\" >0")
    print "clip to project area"
    
    #outCon.save(outlayer)
    #arcpy.Delete_management("Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/temp")

def disthistory(inlayer, distfield, outlayer, fire=False):
    
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    arcpy.AddField_management(inlayer, "DisturbanceYear", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(inlayer, "Age_2010", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    print "added fields"


    arcpy.CalculateField_management(inlayer, "DisturbanceYear", "".join(["int(!",distfield,"!)"]), "PYTHON", "")
    print "calc field 1 done"
    arcpy.CalculateField_management(inlayer, "Age_2010", "2010 - !DisturbanceYear!", "PYTHON", "")
    print "calc field 2 done"

    if fire:
        rule = "\"BEST_ASSESS\" = 'YES' AND \"BURNSEV\" = 4"
        arcpy.SelectLayerByAttribute_management(inlayer, "NEW_SELECTION", rule)
    
        print "selected layer"

    # Set local variables
    assignmentType = "MAXIMUM_AREA"
    cellSize = "30"
    arcpy.PolygonToRaster_conversion(inlayer, "Age_2010", outlayer, assignmentType, "NONE", cellSize)    

    print "conversion successful"

    
def roundto5(inlayer, outlayer):

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    outRaster = Int(Int(Raster(inlayer) + 2.5) / 5 ) * 5
    outRaster.save(outlayer)

def addjoindata(inlayer, joinfield):
    """
    Be sure to join your raster layer to the appropriate csv table before executing this function.
    This function has a lot of hardwired variables; review before using!
        coverfield = 'CoverRaster8_052'
    """
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # add new fields to store info from joined table
    arcpy.AddField_management(inlayer, "J_CoverCode", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(inlayer, "J_ConditionCode", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(inlayer, "J_MaxAge", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    arcpy.AddField_management(inlayer, "Early", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    
    print "fields added"

    #arcpy.AddJoin_management(inlayer, "".join(['"VAT_', inlayer, '.', joinfield, '"']), "Y:/Tahoe/Age/earlyagelookuptable20140605.csv", "CoverCode", "KEEP_ALL")
    arcpy.AddJoin_management(inlayer, joinfield, "Y:/Tahoe/Age/earlyagelookuptable20140605.csv", "CoverCode", "KEEP_ALL")
    print "joined tables"

    #cover = "".join(['"!VAT_', inlayer, ' .J_CoverCode!"'])
    #cover = u"".join(['"VAT_', inlayer, '.J_CoverCode"'])
    #cond = u"".join(['"VAT_', inlayer, '.J_ConditionCode"' ])
    #maxage = u"".join(['"VAT_', inlayer, '.J_MaxAge"'])
    cover = "".join(['VAT_', inlayer, '.J_CoverCode'])
    cond = "".join(['VAT_', inlayer, '.J_ConditionCode' ])
    maxage = "".join(['VAT_', inlayer, '.J_MaxAge'])

    #cond = "".join([' "! ', condfield, ' !" '])
    #maxage = "".join([' "! ', coverfield, ' !" '])

    arcpy.CalculateField_management(inlayer, cover, "!CoverCode!", "PYTHON_9.3", "")
    arcpy.CalculateField_management(inlayer, cond, "!ConditionCode!", "PYTHON_9.3", "")
    arcpy.CalculateField_management(inlayer, maxage, "!MaxAge!", "PYTHON_9.3", "")
    print "fields calculated"

    arcpy.RemoveJoin_management(inlayer, "earlyagelookuptable20140605.csv")

    
    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

def parsecombage(inlayer, agefield, condfield, coverfield):
    """
    For first run:
    agefield = 'AgeRaster_Distur'
    condfield = 'Condition_0601_2'
    coverfield = 'CoverRaster8_052'
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    
    print "making cursor"
    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    # rule is that if the age is less than the max age for that cover type in early, 
    # revise the condition raster to be early
    for row in rows:
        # pull out aspen types first:
        if row.getValue(coverfield) in (6,16,22,23,31):
            if row.getValue(condfield) != 40:
                if row.getValue(agefield) < row.getValue("J_MaxAge"):
                    row.Early = 2
                else:
                    row.Early = 0
        else:
            if row.getValue(condfield) > 10:
                if row.getValue(agefield) < row.getValue("J_MaxAge"):
                    row.Early = 1
                else:
                    row.Early = 0
            else:
                row.Early = 0
        rows.updateRow(row)

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows
    
    #arcpy.Delete_management("featurelayer")

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

def burnage(origagelayer, newagelayer, outlayer):

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    outCon = Con(newagelayer, newagelayer, origagelayer, "\"Value\" < 100")
    outCon.save(outlayer)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")


def burnearly(origcondlayer, earlylayer, outlayer):

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    aspen = 40
    early = 10

    outCon = Con(Raster(earlylayer) == 2, aspen, Con(Raster(earlylayer) == 1, early, origcondlayer))
    outCon.save(outlayer)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

def midages(inlayer):
    
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    cov = 'CoverRaster8_052'

    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        if row.getValue(cov) in (3,7):
            row.Age = 10
        elif row.getValue(cov) in (5,9,11,24):
            row.Age = 20
        elif row.getValue(cov) in (6,12,16,22,23,31):
            row.Age = 5
        elif row.getValue(cov) in (10,21,26,30):
            row.Age = 40
        elif row.getValue(cov) in (13,20):
            row.Age = 30
        elif row.getValue(cov) in (14,):
            row.Age = 25
        elif row.getValue(cov) in (15,):
            row.Age = 50
        elif row.getValue(cov) in (17,29):
            row.Age = 35
        elif row.getValue(cov) in (18,19,25):
            row.Age = 75
        else:
            row.Age = 99998
        rows.updateRow(row)






