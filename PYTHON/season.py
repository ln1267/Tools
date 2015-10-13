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

Var="FWPT_"
path="D:\\AU\\AWAP\\"+"FWPT"+"\\mon\\tif\\"+Var
outpath="D:\\AU\\AWAP\\season\\"


for n in xrange(2000,2013):
    
    b="%02d" % (n+1)
    n="%02d" % n
    
    # Local variables:
    NDVI_1 = path+n+"01.tif"
    NDVI_2 = path+n+"02.tif"
    NDVI_3 = path+n+"03.tif"
    NDVI_4 = path+n+"04.tif"
    NDVI_5 = path+n+"05.tif"
    NDVI_6 = path+n+"06.tif"
    NDVI_7 = path+n+"07.tif"
    NDVI_8 = path+n+"08.tif"
    NDVI_9 = path+n+"09.tif"
    NDVI_10 = path+n+"10.tif"
    NDVI_11 = path+n+"11.tif"
    NDVI_12 = path+n+"12.tif"
    NDVI_2_1 = path+b+"01.tif"
    NDVI_2_2 = path+b+"02.tif"
    NDVI_2_3 = path+b+"03.tif"
    NDVI_2_4 = path+b+"04.tif"
    NDVI_2_5 = path+b+"05.tif"
    NDVI_2_6 = path+b+"06.tif"
    NDVI_2_7 = path+b+"07.tif"
    NDVI_2_8 = path+b+"08.tif"
    NDVI_2_9 = path+b+"09.tif"
    NDVI_2_10 = path+b+"10.tif"
    NDVI_2_11 = path+b+"11.tif"
    NDVI_2_12 = path+b+"12.tif"

    N_wet_tif = outpath+Var+n+"_N_wet.tif"
    S_dry_tif = outpath+Var+n+"_S_dry.tif"
    N_dry_tif = outpath+Var+n+"_N_dry.tif"
    S_wet_tif = outpath+Var+n+"_S_wet.tif"
    # Process: Cell Statistics
    arcpy.gp.CellStatistics_sa([NDVI_4,NDVI_5,NDVI_6,NDVI_7,NDVI_8,NDVI_9,NDVI_10,NDVI_11], S_wet_tif, "SUM", "DATA")
    arcpy.gp.CellStatistics_sa([NDVI_5,NDVI_6,NDVI_7,NDVI_8,NDVI_9], N_dry_tif, "SUM", "DATA")
    arcpy.gp.CellStatistics_sa([NDVI_12,NDVI_2_1,NDVI_2_2,NDVI_2_3], S_dry_tif, "SUM", "DATA")
    arcpy.gp.CellStatistics_sa([NDVI_10,NDVI_11,NDVI_12,NDVI_2_1,NDVI_2_2,NDVI_2_3,NDVI_2_4], N_wet_tif, "SUM", "DATA")

    print n,b
 