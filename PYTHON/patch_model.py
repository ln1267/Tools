# ---------------------------------------------------------------------------
# model.py
# Created on: 2011-11-29 21:40:22.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy


# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")
print "welldone!"

#循环开始
for n in xrange(1962,1963):
	for m in xrange(8,9):
		m="%02d" % m
# Local variables:	
		mulu1="E:\\降水\\"
		mulu2="D:\\output\\"
		mulu3="D:\\output\\"
		mulu4="D:\\output\\minjiang\\"
		mulu5="D:\\output\\zagunao\\"
		name1="tm"+str(n)+m+".txt"
		name2="tm"+str(n)+m+".tif"
		name3="tm"+str(n)+m+".tif"
		name4="tm"+str(n)+m+".dbf"
		inputfile = mulu1+name1
		output_asctoras = mulu2+name2
		output_pro=mulu3+name3
		output_sta_minjiang=mulu4+name4
		output_sta_zagunao=mulu5+name4
		print inputfile,output_asctoras,output_pro,output_sta_minjiang,output_sta_zagunao
		minjiang_shp = "E:\\Data\\岷江流域\\STREAM\\stream.shp"
		zagunao_shp = "E:\\Data\\岷江流域\\STREAM\\zagunao.shp"

# Process: ASCII 转栅格
		arcpy.ASCIIToRaster_conversion(inputfile, output_asctoras, "FLOAT")
		print "成功将ASCII文件转换为Raster" 
		
# Process: 定义投影
		arcpy.DefineProjection_management(output_asctoras, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")
		print "成功转换投影！"

# Process: 以表格显示分区统计岷江流域
		arcpy.gp.ZonalStatisticsAsTable_sa(minjiang_shp, "GRIDCODE", output_pro, output_sta_minjiang, "DATA", "ALL")
		print "成功对岷江流域栅格进行统计"
# Process: 以表格显示分区统计杂谷脑流域
		arcpy.gp.ZonalStatisticsAsTable_sa(zagunao_shp, "OBJECTID_1", output_pro, output_sta_zagunao, "DATA", "ALL")
		print "成功对杂谷脑流域栅格进行统计"
# Process: 删除
		arcpy.Delete_management(output_pro, "RasterDataset")
		print "成功删除TIF影像文件"

