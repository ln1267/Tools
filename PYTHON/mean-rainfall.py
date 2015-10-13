# ---------------------------------------------------------------------------
# mean.py
# Created on: 2014-11-17 12:51:28.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")
for n in xrange(1,13):
    n="%02d" % n

    # Local variables:
    NDVI_1 = "F:\\RAIN\\RAIN-2000"+n+".tif"
    NDVI_2 = "F:\\RAIN\\RAIN-2001"+n+".tif"
    NDVI_3 = "F:\\RAIN\\RAIN-2002"+n+".tif"
    NDVI_4 = "F:\\RAIN\\RAIN-2003"+n+".tif"
    NDVI_5 = "F:\\RAIN\\RAIN-2004"+n+".tif"
    NDVI_6 = "F:\\RAIN\\RAIN-2005"+n+".tif"
    NDVI_7 = "F:\\RAIN\\RAIN-2006"+n+".tif"
    NDVI_8 = "F:\\RAIN\\RAIN-2007"+n+".tif"
    NDVI_9 = "F:\\RAIN\\RAIN-2008"+n+".tif"
    NDVI_10 = "F:\\RAIN\\RAIN-2009"+n+".tif"

    mean_tif = "F:\\RAIN\\"+n+"-RAIN-mean.tif"
		
    # Process: Cell Statistics
    arcpy.gp.CellStatistics_sa([NDVI_1,NDVI_2,NDVI_3,NDVI_4,NDVI_5,NDVI_6,NDVI_7,NDVI_8,NDVI_9,NDVI_10], mean_tif, "mean", "DATA")
		
    print n
	 
