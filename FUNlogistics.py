# FUNlogistics
import arcpy
from arcpy.sa import Exp
from arcpy.sa import Raster

def log4param(inlayer, type, outlayer):
    # set environment settings
    arcpy.env.snapRaster = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.extent = "Y:/Tahoe/GISdata/Lattice_Clip30m.gdb/Lattice_Clip30m_ProjBound"
    arcpy.env.workspace = "Y:/Tahoe/GISdata/WorkGDBCreated051214.gdb/"

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("spatial")

    x = inlayer
    #treatment suitability - slope
    #curve(logistic4p(x,1,.2,.2,30),0,100,ylim=c(0,1),
    #xlab='slope (%)',ylab='probability')

    if type=="slope":
        left = 1
        right = 0.2
        slope = 0.2
        inflexion = 30
        
        #rule = "".join([   str(left), "+((", str(right), "-", str(left), ")/(1+Exp(", str(slope), "*(", str(inflexion), "-", x, "))))"])
        #arcpy.RasterCalculator(rule,outlayer)
        
    outRas = left + ((right-left)/(1+ Exp(slope*(inflexion-Raster(x)))))
    outRas.save(outlayer)

##  #4 parameter logistic 
##  logistic4p<-function(x,left,right,slope,inflexion){
##    left + ((right-left)/(1+exp(slope*(inflexion-x))))
##  }
##  
##  #susceptibility - TPI
##  curve(logistic4p(x,.8,1.2,1,0),-3,3,ylim=c(.8,1.2),
##    xlab='topographic position index',ylab='probability')
##  
##  #treatment suitability - TPI
##  curve(logistic4p(x,.4,1,1,0),-3,3,ylim=c(0,1),
##    xlab='topographic position index',ylab='probability')
##  
##  #treatment suitability - slope
##  curve(logistic4p(x,1,.2,.2,30),0,100,ylim=c(0,1),
##      xlab='slope (%)',ylab='probability')
##  