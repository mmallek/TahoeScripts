# FUNvegsuitability
import arcpy
from arcpy import env
from arcpy.sa import *

def parse_wuipac(inlayer, WUIfield, PACfield):
    """
    Use after combining WUI and PAC rasters.
    """

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # add new field to store results (may need to comment out if redoing code below)
    arcpy.AddField_management(inlayer, "Category", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        # prioritize the WUI assignments, since they are more meaningful as integers
        if row.getValue(PACfield) == 99999:
            row.Category = row.getValue(WUIfield)
        elif row.getValue(WUIfield) < 10 and row.getValue(PACfield) == 1:
            row.Category = row.getValue(WUIfield) * 10
        elif row.getValue(PACfield) == 1 and row.getValue(WUIfield) == 99999:
            row.Category = 6
        rows.updateRow(row)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

def wuipac(inlayer, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.cellSize = 30
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    outCon = Con( ( (inlayer >= 10) & (inlayer <= 50) ), 1) 
    outCon.save(outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")

def vegmin(type, outlayer):
    """ inlayers should be entered in list form ([]) 
    first populate input layers
    """
	
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    slope = 'SlopeLogistic_0528_2157'
    wuipac02 = 'VTC_WUI_PAC_02_0731_1240'
    wuipac04 = 'VTC_WUI_PAC_04_0731_1240'
    wuipac06 = 'VTC_WUI_PAC_06_0731_1240'
    nonwuipac00 = 'VTC_nonWUI_PAC_00_0731_2154'
    nonwuipac04 = 'VTC_nonWUI_PAC_04_0718_1745'
    threatnopac08 = 'VTC_Threat_noPAC_08_0717_1635'
    defensenopac10 = 'VTC_Defense_noPAC_10_0718_1813'
    urbannopac10 = 'VTC_Urban_noPAC_10_0717_1821'
    nonwuinopac06 = 'VTC_nonWUI_noPAC_06_0718_2222'
    nonwuinopac08 = 'VTC_nonWUI_nonPAC_08_0718_1630'
    rca00 = 'VTC_RCAs_00_0731_2206'
    rca06 = 'VTC_RCAs_06_0731_2215'
    tpi = 'VTC_TPI_0718_1652'
    roadproxmech = 'VTC_RoadProx_Mech2_0718_1800'
    roadproxburn = 'VTC_RoadProx_Burn2_0718_1805'
    roadless00 = 'VTC_Roadless_00_0718_1638'


    if type == "Mastication":
        inlayers = [slope, wuipac06, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac06, rca00, tpi, 
        roadproxmech, roadless00]

    elif type == "MastFire":
        inlayers = [slope, wuipac06, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac06, rca00, tpi, 
        roadproxmech, roadless00]

    elif type == "RxFire": # slope layer not included
        inlayers = [wuipac06, nonwuipac04, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca06, tpi, 
        roadproxburn, roadless00]     

    elif type == "Thinning":
        inlayers = [slope, wuipac04, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, 
        roadproxmech, roadless00]            

    elif type == "MatrixThinGroupCut":
        inlayers = [slope, wuipac02, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, 
        roadproxmech, roadless00]            

    elif type == "ThinRxFire":
        inlayers = [slope, wuipac04, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, 
        roadproxmech, roadless00]            

    elif type == "MatrixThinGroupCutRxFire":
        inlayers = [slope, wuipac02, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, 
        roadproxmech, roadless00]   

    elif type == "ThinMastRxFire":
        inlayers = [slope, wuipac02, nonwuipac00, 
        threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, 
        roadproxmech, roadless00]     


    # Calculate minimum
    outCellStats = CellStatistics(inlayers, "MINIMUM", "DATA")

    # Save output
    outCellStats.save(outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")

def vegarithmean(type, outlayer):
    """ inlayers should be entered in list form ([]) """
    
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    slope = 'SlopeLogistic_0528_2157'
    wuipac02 = 'VTC_WUI_PAC_02_0731_1240'
    wuipac04 = 'VTC_WUI_PAC_04_0731_1240'
    wuipac06 = 'VTC_WUI_PAC_06_0731_1240'
    nonwuipac00 = 'VTC_nonWUI_PAC_00_0731_2154'
    nonwuipac04 = 'VTC_nonWUI_PAC_04_0718_1745'
    threatnopac08 = 'VTC_Threat_noPAC_08_0717_1635'
    defensenopac10 = 'VTC_Defense_noPAC_10_0718_1813'
    urbannopac10 = 'VTC_Urban_noPAC_10_0717_1821'
    nonwuinopac06 = 'VTC_nonWUI_noPAC_06_0718_2222'
    nonwuinopac08 = 'VTC_nonWUI_nonPAC_08_0718_1630'
    rca00 = 'VTC_RCAs_00_0731_2206'
    rca06 = 'VTC_RCAs_06_0731_2215'
    tpi = 'VTC_TPI_0718_1652'
    roadproxmech = 'VTC_RoadProx_Mech2_0718_1800'
    roadproxburn = 'VTC_RoadProx_Burn2_0718_1805'
    roadless00 = 'VTC_Roadless_00_0718_1638'

    if type == "Mastication":
        inlayers = [slope, wuipac06, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac06, rca00, tpi, roadproxmech, roadless00]

    elif type == "MastFire":
        inlayers = [slope, wuipac06, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac06, rca00, tpi, roadproxmech, roadless00]

    elif type == "RxFire": # slope layer not included
        inlayers = [wuipac06, nonwuipac04, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca06, tpi, roadproxmech, roadless00]     

    elif type == "Thinning":
        inlayers = [slope, wuipac04, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, roadproxmech, roadless00]            

    elif type == "MatrixThinGroupCut":
        inlayers = [slope, wuipac02, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, roadproxmech, roadless00]            

    elif type == "ThinRxFire":
        inlayers = [slope, wuipac04, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, roadproxmech, roadless00]            

    elif type == "MatrixThinGroupCutRxFire":
        inlayers = [slope, wuipac02, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, roadproxmech, roadless00]   

    elif type == "ThinMastRxFire":
        inlayers = [slope, wuipac02, nonwuipac00, threatnopac08, defensenopac10, urbannopac10, 
        nonwuinopac08, rca00, tpi, roadproxmech, roadless00]     


    # Calculate arithmetic mean
    outCellStats = CellStatistics(inlayers, "MEAN", "DATA")

    # Save output
    outCellStats.save(outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")


def vegbackground(inlayer, outlayer):
    """This function converts rasters where the background is "no data" to a raster where the background
    is 1 within the project boundary and "no data" elsewhere. Necessary to calculate the geometric mean,
    since that requires raster algebra"""

    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    # convert background
    outCon = Con(IsNull(inlayer) == 1, 1, inlayer)
    outCon.save(outlayer)


    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("Spatial")


def veggeommean(type, outlayer):
    """ 
    inlayers should be entered in list form ([]) 
    geometric mean is the nth root of the product of n cells
    """
    
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.cellSize = 30
    
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    slope = 'SlopeLogistic_0528_2157'
    wuipac02 = 'bVTC_WUI_PAC_02_0804_2251'
    wuipac04 = 'bVTC_WUI_PAC_04_0804_2253'
    wuipac06 = 'bVTC_WUI_PAC_06_0804_2255'
    nonwuipac00 = 'bVTC_nonWUI_PAC_00_0804_2258'
    nonwuipac04 = 'bVTC_nonWUI_PAC_04_0721_2114'
    threatnopac08 = 'bVTC_Threat_noPAC_08_0721_2116'
    defensenopac10 = 'bVTC_Defense_noPAC_10_0721_2117'
    urbannopac10 = 'bVTC_Urban_noPAC_10_0721_2118'
    nonwuinopac06 = 'bVTC_nonWUI_noPAC_06_0721_2119'
    nonwuinopac08 = 'bVTC_nonWUI_nonPAC_08_0721_2118'
    rca00 = 'bVTC_RCAs_00_0804_2259'
    rca06 = 'bVTC_RCAs_06_0804_2101'
    tpi = 'VTC_TPI_0718_1652'
    roadproxmech = 'VTC_RoadProx_Mech2_0718_1800'
    roadproxburn = 'VTC_RoadProx_Burn2_0718_1805'
    roadless00 = 'bVTC_Roadless_00_0721_2122'

    threatnopac = threatnopac08
    defensenopac = defensenopac10
    urbannopac = urbannopac10 
    tpi = tpi   
    roadless = roadless00

    if type == "Mastication":
        #slope = slope
        wuipac = wuipac06
        nonwuipac = nonwuipac00
        #threatnopac = threatnopac08
        #defensenopac = defensenopac10
        #urbannopac = urbannopac10
        nowuinopac = nonwuinopac06
        rcas = rca00
        #tpi = tpi
        roadprox = roadproxmech
        #roadless = roadless00

    elif type == "MastFire":
        wuipac = wuipac06
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac06
        rcas = rca00
        roadprox = roadproxmech

    elif type == "RxFire": # slope layer not included
        wuipac = wuipac06
        nonwuipac = nonwuipac04
        nowuinopac = nonwuinopac08
        rcas = rca06
        roadprox = roadproxburn
  
    elif type == "Thinning":
        wuipac = wuipac04
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac08
        rcas = rca00
        roadprox = roadproxmech

    elif type == "ThinRxFire":
        wuipac = wuipac04
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac08
        rcas = rca00
        roadprox = roadproxmech 

    elif type == "MatrixThinGroupCut":
        wuipac = wuipac02
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac08
        rcas = rca00
        roadprox = roadproxmech

    elif type == "MatrixThinGroupCutRxFire":
        wuipac = wuipac02
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac08
        rcas = rca00
        roadprox = roadproxmech

    elif type == "ThinMastRxFire":
        wuipac = wuipac02
        nonwuipac = nonwuipac00
        nowuinopac = nonwuinopac08
        rcas = rca00
        roadprox = roadproxmech  


    # Calculate geometric mean
    if type == "RxFire":
        numlayers = 10
        root = 1.0/numlayers
        outRaster = (Raster(wuipac) * Raster(nonwuipac) * Raster(threatnopac) * Raster(defensenopac) * 
            Raster(urbannopac) * Raster(nowuinopac) * Raster(rcas) * Raster(tpi) * Raster(roadprox) * 
            Raster(roadless) ** root)
    else:
        numlayers = 11
        root = 1.0/numlayers
        outRaster = (Raster(slope) * Raster(wuipac) * Raster(nonwuipac) * Raster(threatnopac) * Raster(defensenopac) * 
            Raster(urbannopac) * Raster(nowuinopac) * Raster(rcas) * Raster(tpi) * Raster(roadprox) * 
            Raster(roadless) ** root)

    # Save output
    outRaster.save(outlayer)

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")
