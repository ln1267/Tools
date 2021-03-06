# ---------------------------------------------------------------------------
# 计算器求均值.py
# Created on: 2011-12-17 10:15:36.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")

for n in xrange(2001,2002):
#	for m in xrange(1,13):
#		m=range(1,13)
#		m="%02d" % m
		i=0
		for a in xrange(1,365,8):
			a="%03d" % a
			m=range(1,13)
#			m="%02d" % m
			d=range(1,47)
			input_tif=range(1,48)
			d[i]=a
			print d[i]
			# Local variables:
			input_mulu="E:\10.11.211.9\convers\"
			input_tif[i+1] = input_mulu+"MOD15A2.A"+str(n)+str(d[i])+".Lai_1km.tif"
			print input_tif[i+1]
			i=i+1
			output_tif=range(1,13)
			for list in xrange(0,12):
				list="%02d" % list
				output_tif[list]=output_mulu+"MOD15A2.A"+str(n)+str(list)+".Lai_1km.tif"
				print output_tif
		# Process: 栅格计算器
		#arcpy.gp.RasterCalculator_sa("(\"%input[0]%\" + \"%input[1]%\" + \"%input[2]%\" + \"%input[3]%\") / 4.0", output_tif[0])
		#arcpy.gp.RasterCalculator_sa("(\"%input[4]%\" + \"%input[5]%\" + \"%input[6]%\" + \"%input[7]%\") / 4.0", output_tif[1])
		#arcpy.gp.RasterCalculator_sa("(\"%input[8]%\" + \"%input[9]%\" + \"%input[10]%\" + \"%input[11]%\") / 4.0", output_tif[2])
		#arcpy.gp.RasterCalculator_sa("(\"%input[12]%\" + \"%input[13]%\" + \"%input[14]%\" ) / 3.0", output_tif[3])
		#arcpy.gp.RasterCalculator_sa("(\"%input[15]%\" + \"%input[16]%\" + \"%input[17]%\" + \"%input[18]%\") / 4.0", output_tif[4])
		#arcpy.gp.RasterCalculator_sa("(\"%input[19]%\" + \"%input[20]%\" + \"%input[21]%\" + \"%input[22]%\") / 4.0", output_tif[5])
		#arcpy.gp.RasterCalculator_sa("(\"%input[23]%\" + \"%input[24]%\" + \"%input[25]%\" + \"%input[26]%\") / 4.0", output_tif[6])
		#arcpy.gp.RasterCalculator_sa("(\"%input[27]%\" + \"%input[28]%\" + \"%input[29]%\" + \"%input[30]%\") / 4.0", output_tif[7])
		#arcpy.gp.RasterCalculator_sa("(\"%input[31]%\" + \"%input[32]%\" + \"%input[33]%\" + \"%input[34]%\") / 4.0", output_tif[8])
		#arcpy.gp.RasterCalculator_sa("(\"%input[35]%\" + \"%input[36]%\" + \"%input[37]%\") / 3.0", output_tif[9])
		#arcpy.gp.RasterCalculator_sa("(\"%input[38]%\" + \"%input[39]%\" + \"%input[40]%\" + \"%input[41]%\") / 4.0", output_tif[10])
		#arcpy.gp.RasterCalculator_sa("(\"%input[42]%\" + \"%input[43]%\" + \"%input[44]%\" + \"%input[45]%\") / 4.0", output_tif[11])
	for h in xrange(1,13):
		h=range(1,13)
		h="%02d" % h
		# Local variables:
		in_tif = "MOD15A2.A"+str(n)+str(h)+".Lai_1km.tif"
		output_extra = "MOD15A2.A"+str(n)+str(h)+".Lai_1km.tif"
		stream_shap = "stream_Identity_dossolve"
		out_dbf = "MOD15A2.A"+str(n)+str(h)+".Lai_1km.tif"
		# Process: 定义投影
		arcpy.DefineProjection_management(in_tif, "PROJCS['AEA_WGS_1984',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['false_easting',0.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',105.0],PARAMETER['standard_parallel_1',25.0],PARAMETER['standard_parallel_2',47.0],PARAMETER['latitude_of_origin',0.0],UNIT['Meter',1.0]]")

		# Process: 按属性提取
		arcpy.gp.ExtractByAttributes_sa(in_tif, "\"value\" < 100", output_extra)

		# Process: 以表格显示分区统计
		arcpy.gp.ZonalStatisticsAsTable_sa(stream_shap, "FID", output_extra, out_dbf, "DATA", "MEAN")
