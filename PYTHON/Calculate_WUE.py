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
#	for m in xrange(1,13):
#		m="%02d" % m		#转换为两位日期
# Local variables:	
												#如果处理气象数据统计字段为"HUC_8"，统计LAI是则为”HUC_8"
		data_class="modet"							#待处理的数据类型”TM“，”PRE“，”LAI“
		data_class2="basin"					#判断是处理气象数据还是LAI数据；分别用“basin_"和“VEG_"

		name2=str(n)+".tif"

		
######################-----待预处理的气象数据文件------##############################################		
		input_file1 = "K:\\Data\\MODIS\\MOD17A2\\MOD17_0.05\\annual\\MODGPP"+name2
		input_file2 = "K:\\Data\\MODIS\\MOD16A2\\MODET\\float\\MODET"+name2
		output_file="K:\\Data\\MODIS\\WUE\\0.05_annual\\WUE"+name2
################################################################################
	
		
		print input_file1,output_file
		
##########################待处理流域边界文件##############################################		

		
		# Process: 以表格显示分区统计岷江流域
# Process: Divide
		arcpy.gp.Divide_sa(input_file1, input_file2, output_file)
		print n
