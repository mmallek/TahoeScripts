# FUNmodifiers
import arcpy
from arcpy import env
from arcpy.sa import *
# xeric-mesic gradient file is called:
# ASP_PET_STOR_TWI_FM5_0626_1440
# mean = -0.001582074106782203
# sd = 2.042811110018231

# for RFR, SMC, break point was 'mean - 1/4 sd'
# -0.5122848516
# for MEG, break point is 'mean - 1/2 sd'
# -1.0229876291

# use FUNpolytoraster to convert ultramafic layer

def makemods(gradientlayer, um, outlayer):
    """
    Gradient layer location: Y:/Tahoe/GISdata/ScratchRaster7.gdb/ASP_PET_STOR_TWI_FM5_0626_1440
    FUNmodifiers.makemods("Y:/Tahoe/GISdata/ScratchRaster7.gdb/ASP_PET_STOR_TWI_FM5_0626_1440", "Ultramafic_0523_2322", "Modifiers_0525_1158")
    """
    
    # not sure first half works...

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    # set environment settings
    env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # um layer will be 0=non-um, 1=um
    xm = gradientlayer
    
    # 2 = mesic, 3 = xeric
    # 4 = mesic, 5 = xeric
    arcpy.gp.Con_sa(gradientlayer, "2", "smc", "3", "VALUE > -0.5122848516")
    arcpy.gp.Con_sa(gradientlayer, "4", "meg", "5", "VALUE > -1.0229876291") 
    print "Cons completed"

    smc = "smc"
    meg = "meg"

    #out = arcpy.gp.Combine([um, smc, meg])
    arcpy.gp.Combine_sa(';'.join([smc, meg, um]), outlayer)
    #out.save(outlayer)
    #out = Combine([umlayer, smc_mesic, meg_mesic])
    print "combine succeeded"

def parsemodcombine(inlayer, smc, meg, um):
    # um = Ultramafic999_05
    # add new field to store results (may need to comment out if redoing code below)
    arcpy.AddField_management(inlayer, "Mod", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    print "add field succeeded"

    # create update cursor for feature class
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        #first priority is um
        if row.getValue(um) == 1:
            # ultramafic status overrides everything else
            row.Mod = 1
        elif row.getValue(smc) == 2 and row.getValue(meg) == 4 and row.getValue("Mod") is None:
            # these cells are mesic no matter what
            row.Mod = 2
        elif row.getValue(smc) == 3 and row.getValue(meg) == 5 and row.getValue("Mod") is None:
            # these cells are xeric no matter what
            row.Mod = 3
        elif row.getValue(smc) == 2 and row.getValue(meg) == 5 and row.getValue("Mod") is None:    
            # smc_mesic but meg_xeric
            row.Mod = 4
        elif row.getValue(smc) == 3 and row.getValue(meg) == 4 and row.getValue("Mod") is None:    
            # smc_xeric but meg_mesic
            row.Mod = 5
        rows.updateRow(row)

    print "populated new row"

    # save the output
    #out.save(outlayer)

    # Check in the ArcGIS Spatial Analyst extension license
    arcpy.CheckInExtension("spatial")

    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows


#FUNmodifiers.makemods("ASP_PET_STOR_TWI_FM5_0626_1440", Ultramafic999_0525_2255", "Modifiers_0525_2258")

def parsemodcombine2(inlayer, cover, mod, outlayer):
    """ This function calculates the new landcover layers that include 
    mesic, xeric, and ultramafic varieties of some cover types.
    cover and mod are the fields in a combine output that will be used to populate the new field "Cover".
    """
    # add field
    arcpy.AddField_management(inlayer, "Landcover1", "TEXT", "", "", "10", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(inlayer, "Landcover2", "TEXT", "", "", "10", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(inlayer, "Landcover3", "TEXT", "", "", "10", "", "NULLABLE", "NON_REQUIRED", "")
    print "add field succeeded"
    

    # create update cursor for feature class
    # Mod defs: 1=um, 2=mesic, 3=xeric, 4=smc_mesic, 5=smc_xeric
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        # these set Landcover2 as prep for concatenation
        if row.getValue(mod) == 1 and row.getValue(cover) in (1,2,3,14):
            row.Landcover2 = "_U"
        if row.getValue(mod) ==  2 and row.getValue(cover) in (1,3,14):
            row.Landcover2 = "_M"
        if row.getValue(mod) == 3 and row.getValue(cover) in (1,3,14):
            row.Landcover2 = "_X"
        if row.getValue(mod) == 4 and row.getValue(cover) in (3,14):
            row.Landcover2 = "_M"
        if row.getValue(mod) == 4 and row.getValue(cover) in (1,):
            row.Landcover2 = "_X"
        if row.getValue(mod) == 5 and row.getValue(cover) in (3,14):
            row.Landcover2 = "_X"
        if row.getValue(mod) == 5 and row.getValue(cover) in (1,):
            row.Landcover2 = "_M"
#        elif row.getValue(Landcover2) is None:
#            row.Landcover2 = ""
        # these set cover types as text
        if row.getValue(cover) == 1:
            row.Landcover1 = "MEG"
        if row.getValue(cover) == 2:
            row.Landcover1 = "OCFW"
        if row.getValue(cover) == 3:
            row.Landcover1 = "SMC"
        if row.getValue(cover) == 5:
            row.Landcover1 = "GRASS"            
        if row.getValue(cover) == 6:
            row.Landcover1 = "OAK"            
        if row.getValue(cover) == 7:
            row.Landcover1 = "BAR"
        if row.getValue(cover) == 9:
            row.Landcover1 = "WAT"
        if row.getValue(cover) == 10:
            row.Landcover1 = "URB"
        if row.getValue(cover) == 11:
            row.Landcover1 = "AGR"
        if row.getValue(cover) == 12:
            row.Landcover1 = "MRIP"
        if row.getValue(cover) == 13:
            row.Landcover1 = "MED"
        if row.getValue(cover) == 14:
            row.Landcover1 = "RFR"
        if row.getValue(cover) == 15:
            row.Landcover1 = "LPN"
        if row.getValue(cover) == 16:
            row.Landcover1 = "SCN"
        if row.getValue(cover) == 17:
            row.Landcover1 = "WWP"
        if row.getValue(cover) == 18:
            row.Landcover1 = "SCN"
        if row.getValue(cover) == 19:
            row.Landcover1 = "SAGE"
        if row.getValue(cover) == 20:
            row.Landcover1 = "CMM"
        if row.getValue(cover) == 21:
            row.Landcover1 = "LSG"
        if row.getValue(cover) == 30:
            row.Landcover1 = "SMC_ASP"
        if row.getValue(cover) == 31:
            row.Landcover1 = "RFR_ASP"
        if row.getValue(cover) == 32:
            row.Landcover1 = "LPN_ASP"
        if row.getValue(cover) == 33:
            row.Landcover1 = "SCN_ASP"
        if row.getValue(cover) == 34:
            row.Landcover1 = "YPN_ASP"            
        rows.updateRow(row)
        print "updated rows"
    
    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

    # next populate column 3
    rows = arcpy.UpdateCursor(inlayer)
    for row in rows:
        if row.getValue("Landcover2") is None:
            row.Landcover3 = row.Landcover1
        else:
            row.Landcover3 = "".join([row.Landcover1, row.Landcover2])
        rows.updateRow(row)
    # Regardless of whether the script succeeds or not, delete the row and cursor
    if row:
        del row
    if rows:
        del rows

    





