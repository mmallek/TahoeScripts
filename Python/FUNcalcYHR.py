import arcpy
def calcYHR(EVeg):
	AGR = "'AG'"
	BAR = "'BA','SN'"
	CMM = "'BM'"
	EARLY = "'BX', 'CG','CH','CI','CL','CM','CP','CQ','CS','CV','CW','CX','CY','MN','QQ','QS'"
	GRASS = "'AC','HG','HM'"
	LPN = "'LP'"
	LSG = "'BL','TN'"
	MED = "'HJ', 'HT'"
	MEG = "'DF','NX','QB','QC','QH','QM','QT','QW','TX'"
	MRIP = "'NR','QE','QO','QX','QY','TA','WL'"
	OAK = "'PD','QD','QL'"
	RFR = "'RF'"
	SAGE = "'BB','BS','BQ','TB'"
	SCN = "'AX','MH','SA','WB'"
	SMC = "'MP','DP','MD','MF','PE','WF'"
	URB = "'IC','IG','IS','UB'"
	WAT = "'W1','W2','W3','W8','WA'"
	WWP = "'WW'"
	
	all_yhrs = [AGR, BAR, CMM, EARLY, GRASS, LPN, LSG, MED, MEG, MRIP, OAK, RFR, SAGE, SCN, SMC, URB, WAT, WWP]
	named_yhrs = ['AGR', 'BAR', 'CMM', 'EARLY', 'GRASS', 'LPN', 'LSG', 'MED', 'MEG', 'MRIP', 'OAK', 'RFR', 'SAGE', 'SCN', 'SMC', 'URB', 'WAT', 'WWP']
	yhrdict = dict(zip(named_yhrs, all_yhrs))
	
	EVeg = EVeg
	# normal types
	for name, yhr in yhrdict.items():
	    rule = "\"REGIONAL_DOMINANCE_TYPE_1\" In(" + yhr + ")"
	    arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", rule)
	    #fill = r'"\"%s"\"'%name
	    fill = '"%s"'%name
	    print fill
	    arcpy.CalculateField_management(EVeg, "AYHR10", fill, "PYTHON", "")
	# special types
	# mixed evergreen
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"REGIONAL_DOMINANCE_TYPE_1\" In('DF','NX','QB','QC','HQ','QM','QT','QW','TX') OR \"REGIONAL_DOMINANCE_TYPE_2\" IN('QT')")
	arcpy.CalculateField_management(EVeg, "AYHR10", "'MEG'", "PYTHON", "")
	
	# yellow pine
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"CrestSide2\" = 'East' AND \"REGIONAL_DOMINANCE_TYPE_1\" IN('EP', 'JP', 'PP') AND \"REGIONAL_DOMINANCE_TYPE_2\" Not In ('QK', 'QC', 'CH', 'TX', 'CS')")
	arcpy.CalculateField_management(EVeg, "AYHR10", "'YPN'", "PYTHON", "")
	
	# ocfw 
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"CrestSide2\" = 'East' AND \"REGIONAL_DOMINANCE_TYPE_1\" = 'QK'")
	arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", "\"CrestSide2\" = 'East' AND \"REGIONAL_DOMINANCE_TYPE_1\" In('EP','JP','PP') AND \"REGIONAL_DOMINANCE_TYPE_2\" In('QK','QC','CH','TX','CS')")
	arcpy.SelectLayerByAttribute_management(EVeg, "ADD_TO_SELECTION", "\"CrestSide2\" = 'West' AND \"REGIONAL_DOMINANCE_TYPE_1\" In('EP','JP','PP','QK')")
	arcpy.CalculateField_management(EVeg, "AYHR10", "'OCFW'", "PYTHON", "")
	
	# road
	arcpy.SelectLayerByAttribute_management(EVeg, "NEW_SELECTION", "\"AYHR9\"='ROAD'")
	arcpy.CalculateField_management(EVeg, "AYHR10", "'ROAD'", "PYTHON", "")

	# early
