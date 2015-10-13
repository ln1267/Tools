# ---------------------------------------------------------------------------
# 生成气象数据————————arcgis9.3
# Created on: 星期五 十二月 09 2011 11:27:55 上午
#   (generated by ArcGIS/ModelBuilder)
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")


#循环开始
for n in xrange(2000,2012):
	for m in xrange(1,13):
		m="%02d" % m		#转换为两位日期
# Local variables:	
												#如果处理气象数据统计字段为"HUC_8"，统计LAI是则为”HUC_8"
		data_class="MODGPP"							#待处理的数据类型”TM“，”PRE“，”LAI“
		data_class2="basin"					#判断是处理气象数据还是LAI数据；分别用“basin_"和“VEG_"
##########################-----流域划分个数---------##############################################			
		basin_num_1=""
		################################################################################	
		
##########################-----公用目录---------##############################################		
		input_dir="K:\\Data\\MODIS\\MODGPP\\"	#气象数据所在的目录
		input_dir1="K:\\Data\\MODIS\\MODGPP\\MODGPP\\"
		output_raster_dir="d:\\zagunao\\"+data_class+"\\"			#转换生成的栅格所在的目录
		basins_dir="d:\\zagunao\\" 	#流域边界所在目录
################################################################################		
		
#######################---转换后的每个流域结果的输出目录------#################################
		
		output_data_dir_basin_1=output_raster_dir+basin_num_1	#统计结果输出的目录
		################################################################################

		name1=data_class+str(n)+m+".txt"
		name2=data_class+str(n)+m+".tif"
		name4=data_class+str(n)+m+".dbf"
		
######################-----待预处理的气象数据文件------##############################################		
		input_file = input_dir+name1							#输入气象数据
#		input_file = "d:\\tm196101.txt"
		output_asctoras = input_dir+name2				#处理后的气象数据
		output_asctoras1 = input_dir1+name2				#处理后的气象数据
################################################################################
		
######################-----分流域处理后的结果文件------##############################################		
		output_data_basin_1=output_data_dir_basin_1+name4	#按表格统计结果输出的文件
		################################################################################
		
		print input_dir,output_asctoras,output_data_basin_1
		
##########################待处理流域边界文件##############################################		
		basin_1=basins_dir+data_class2+basin_num_1+".shp"	# 待统计流域边界
################################################################################


		# Process: 条件函数 提取出有效值
		arcpy.gp.Con_sa(output_asctoras, output_asctoras, output_asctoras1, "0", "\"Value\" <=32000 AND \"Value\" >=0")
		# Process: 设为空函数
#		arcpy.gp.SetNull_sa(output_asctoras, output_asctoras, output_asctoras1, "value >32700")
		# Process: 定义投影
#		arcpy.DefineProjection_management(output_asctoras, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")
