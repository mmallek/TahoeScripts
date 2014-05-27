import arcpy
def calcDevelop(EVeg):
    # nulls
    #EVeg = "EVeg_070813_1545"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"OS_TREE_DIAMETER_CLASS_1\" Is Null")
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")
    
    common = "'MEG','SCN','SMC','WWP','YPN','RFR'"
    static = "'AGR','BAR','GRASS','MED','ROAD','URB','WAT'"
    
    # static
    rule = "\"AYHR10\" In(" + static + ")"
    
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Static'", "PYTHON", "")

    print "finished static types"
    
    # type is early
    rule = "\"AYHR10\" = 'EARLY'"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")

    # common
    # early
    rule = "\"AYHR10\" In(" + common + ") And \"OS_TREE_DIAMETER_CLASS_1\" In ('N','00','02','N')"
    
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")
    
    # mid
    rule = "\"AYHR10\" In(" + common + ") And \"OS_TREE_DIAMETER_CLASS_1\" In ('07','15')"
    
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")
    
    # late
    rule = "\"AYHR10\" In(" + common + ") And \"OS_TREE_DIAMETER_CLASS_1\" In ('25','40')"
    
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Late'", "PYTHON", "")
    
    print "finished common types"
    
    # LPN, OAK
    # early
    rule = "\"AYHR10\" In('LPN','OAK') And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")
    
    # mid
    rule = "\"AYHR10\" In('LPN','OAK') And \"OS_TREE_DIAMETER_CLASS_1\" In ('07')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")
    
    # late
    rule = "\"AYHR10\" In('LPN','OAK') And \"OS_TREE_DIAMETER_CLASS_1\" In ('15','25','40')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Late'", "PYTHON", "")

    print "LPN and OAK finished"
    
    # MRIP
    # early
    rule = "\"AYHR10\" = 'MRIP' And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02','07')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")
    
    # mid
    rule = "\"AYHR10\" = 'MRIP' And \"OS_TREE_DIAMETER_CLASS_1\" In ('15')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")
    
    # late
    rule = "\"AYHR10\" = 'MRIP' And \"OS_TREE_DIAMETER_CLASS_1\" In ('25','40')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Late'", "PYTHON", "")

    print "MRIP finished"
    
    # OCFW
    # early
    rule = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02') And \"OS_TREE_DIAMETER_CLASS_2\" In ('00','02')"
    rule2 = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02','N') And \"OS_TREE_DIAMETER_CLASS_2\" Is Null"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", rule2)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")
    
    # mid
    rule = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02') And \"OS_TREE_DIAMETER_CLASS_2\" In ('07','15','25')"
    rule2 = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('07','15','25') And \"OS_TREE_DIAMETER_CLASS_2\" Is Null"
    rule3 = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('07','15','25') And \"OS_TREE_DIAMETER_CLASS_2\" In ('00','02','07','15','25')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", rule2)
    arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", rule3)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")
    
    # late
    rule = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_1\" In ('40')"
    rule2 = "\"AYHR10\" = 'OCFW' And \"OS_TREE_DIAMETER_CLASS_2\" In ('40')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", rule2)
    arcpy.CalculateField_management(EVeg, "Develop", "'Late'", "PYTHON", "")

    # or tdc-2 in 40?

    print "OCFW finished"
    
    # SAGE
    rule = "\"AYHR10\" = 'SAGE' And \"OS_TREE_DIAMETER_CLASS_1\" Is Null"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Early'", "PYTHON", "")

    rule = "\"AYHR10\" = 'SAGE' And \"OS_TREE_DIAMETER_CLASS_1\" Is Not Null"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")

    print "Sage finished"
    
    # LSG
    rule = "\"AYHR10\" = 'LSG' And \"SHB_CFA\" In(' ', '', '00','05','15','25','35','45') "
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Mid'", "PYTHON", "")
    
    rule = "\"AYHR10\" = 'LSG' And \"SHB_CFA\" In('55','65','75','85','95')"
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
    arcpy.CalculateField_management(EVeg, "Develop", "'Late'", "PYTHON", "")

    print "LSG finished"
    
    
    # CMM
    # Polygons will be randomly assigned to the other condition classes based on a 20:10:70 distribution 
    # for early/mid/late development (based on an analysis of past fire in the project area). Random numbers between 0 and 1 were 
    # generated using numpy for Python and used to assign each CMM polygon to a condition.
    
    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"AYHR10\" = 'CMM'")
    expression = "random.choice(['Early']*2 + ['Mid']*1 + ['Late']*7)"
    codeblock = "import random"
    arcpy.CalculateField_management(EVeg, "Develop", expression, "PYTHON", codeblock)

    print "CMM finished"


