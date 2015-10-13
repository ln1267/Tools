# ---------------------------------------------------------------------------
# patch_modis_lai.py
# modis_lai处理：投影转换----提取栅格“<100”的值------利用植被图层分区统计栅格平均值
# Created on: 2011-12-15 20:44:09.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")

for n in xrange(2001,2011):
#	for m in xrange(1,13):
#		m=range(1,13)
#		m="%02d" % m
		i=0
		for d in xrange(1,365,8):
			d="%03d" % d
			input_mulu="E:\\10.11.211.9\\convers\\"			
			input_tif = input_mulu+"MOD15A2.A"+str(n)+d+".Lai_1km.tif"  
			# Local variables:
			out_mulu="d:\\output\\lai\\"
			output_extra = out_mulu+"MOD15A2.A"+str(n)+d+".Lai_1km.extra.tif"
			stream_shap = "E:\\Data\\岷江流域\\STREAM\\stream_Identity_dossolve.shp"
			out_dbf = out_mulu+"dbf\\"+"MOD15A2.A"+str(n)+d+".Lai_1km.dbf"
			print input_tif,output_extra,out_dbf
#Process: 定义投影
			arcpy.DefineProjection_management(input_tif, "PROJCS['AEA_WGS_1984',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['false_easting',0.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',105.0],PARAMETER['standard_parallel_1',25.0],PARAMETER['standard_parallel_2',47.0],PARAMETER['latitude_of_origin',0.0],UNIT['Meter',1.0]]")
			print "成功定义投影"
			
# Process: 按属性提取
			arcpy.gp.ExtractByAttributes_sa(input_tif, "\"value\" < 100", output_extra)
			print "成功提取<100的值"

# Process: 以表格显示分区统计
			arcpy.gp.ZonalStatisticsAsTable_sa(stream_shap, "FID", output_extra, out_dbf, "DATA", "MEAN")
			print "成功进行分区统计"

