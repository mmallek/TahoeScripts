import arcpy
def calcCanopy(EVeg):
	
	yhrs = "'LPN', 'MRIP', 'RFR', 'SCN', 'SMC', 'WWP','YPN'"
	rule1 = " \"AYHR10\" In(" + yhrs + ")"
	
	rule2 = " \"TOTAL_TREE_CFA\" In ('00','05','15','25','35') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	print "generic open complete"

	yhrs = "'MEG','SCN','SMC','WWP','YPN','RFR'"
	rule = "\"AYHR10\" In(" + yhrs + ") And \"OS_TREE_DIAMETER_CLASS_1\" In ('00','02')"
	
	rule2 = "\"TOTAL_TREE_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")

	print "common mixed complete"
	
	rule2 = "\"TOTAL_TREE_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "common closed complete"
	
	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('00','05','15','25','35') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	print "TT Null & CON Open complete"
	
	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")

	print "TT Null & CON Mixed complete"
	
	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "TT Null & CON Closed complete"
	
	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" Is Null "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	print "TT & CON Null complete"

	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" = 'X' And \"TREE_CFA_CLASS_1\" In('X','',' ', '00','01','05','15','20','25','35')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" = 'X' And \"TREE_CFA_CLASS_1\" In('40','45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")

	rule2 = "\"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" = 'X' And \"TREE_CFA_CLASS_1\" In('75','80','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "TT Null and Con X complete"

	rule2 = "\"TOTAL_TREE_CFA\" = 'X' And \"CON_CFA\" Is Null And \"TREE_CFA_CLASS_1\" In('X','',' ', '00','01','05','15','20','25','35') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")
	
	rule2 = "\"TOTAL_TREE_CFA\" = 'X' And \"CON_CFA\" Is Null And \"TREE_CFA_CLASS_1\" In('40','45','55','65') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	rule2 = "\"TOTAL_TREE_CFA\" = 'X' And \"CON_CFA\" Is Null And \"TREE_CFA_CLASS_1\" In('75','80','85','95') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	print "TT is X & Con Null & Alt Tree CFA complete"


	# shrubs
	yhrs = "'CMM', 'LSG', 'SAGE'"
	rule1 = "\"AYHR10\" In(" + yhrs + ")"
	
	rule2 = " \"SHB_CFA\" In ('',' ','00','05','15','25','35') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")

	print "shrub open complete"
	
	rule2 = "\"SHB_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")

	print "shrub mixed complete"
	
	rule2 = "\"SHB_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "shrub closed complete"
	


	print "early complete"

	# types where hardwood cover is relevant
	yhrs = "'OAK', 'OCFW', 'MEG'"
	rule1 = "\"AYHR10\" In(" + yhrs + ")"
	
	# when total tree is sufficient
	rule2 = " \"TOTAL_TREE_CFA\" In ('00','05','15','25','35') "
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")
	
	rule2 = "\"TOTAL_TREE_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")
	
	rule2 = "\"TOTAL_TREE_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "hdw w/TT complete"
	
	# total tree is null
	
	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('00','05','15','25','35')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")
	
	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")
	
	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")

	print "hdw w/TT null complete"
	
	# remaining special cases
	
	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" Is Null And \"HDW_CFA\" In ('00','05','15','25','35')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Open'", "PYTHON", "")


	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"CON_CFA\" Is Null And \"HDW_CFA\" In ('45','55','65')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Mixed'", "PYTHON", "")
	
	rule2 = " \"TOTAL_TREE_CFA\" Is Null And \"HDW_CFA\" In ('75','85','95')"
	rules = rule1 + " And " + rule2
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rules )
	arcpy.CalculateField_management(EVeg, "Canopy", "'Closed'", "PYTHON", "")



	print "remaining special cases complete"

	# everything static will be marked Static
	
	static = "'AGR','BAR','GRASS','MED','ROAD','URB','WAT'"
	
	rule = "\"AYHR10\" In(" + static + ")"    
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
	arcpy.CalculateField_management(EVeg, "Canopy", "'Static'", "PYTHON", "")

	print "static complete"
	
	# type is early
	# everything in Early will be marked All
	
	rule = "\"AYHR10\" = 'EARLY'"
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"AYHR10\" = 'EARLY'")
	arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", "\"Develop\" = 'Early'")
	arcpy.CalculateField_management(EVeg, "Canopy", "'All'", "PYTHON", "")	

