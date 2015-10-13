
data v1 v2 v3 cellinfo cellinfo1 hucarea ;
delete;
run;
/*读取SOIL文件并输出到TXT格式*/
PROC IMPORT OUT= WORK.v1 
            DATAFILE= "E:\HUC\soil_union.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'15$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC EXPORT DATA= WORK.V1 
            OUTFILE= "E:\HUC\Inputs\Inputs_15\soilinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*读取LAI文件并输出到TXT格式*/
PROC IMPORT OUT= WORK.v2 
            DATAFILE= "E:\HUC\lai_union.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'15$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC EXPORT DATA= WORK.V2 
            OUTFILE= "E:\HUC\Inputs\Inputs_15\landlai.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*读取CLIMATE文件并输出到TXT格式*/
PROC IMPORT OUT= WORK.v3 
            DATAFILE= "E:\HUC\basin_15\climate.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
 
PROC EXPORT DATA= WORK.V3 
            OUTFILE= "E:\HUC\Inputs\Inputs_15\climate.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;



/*读取流域basin文件和VEG_RATIO文件并输出CELLINFO和HUCAREA到TXT格式*/
PROC IMPORT OUT= WORK.basin_15 
            DATAFILE= "E:\HUC\basins\basin_15.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

PROC IMPORT OUT= WORK.Vatio_15 
            DATAFILE= "E:\HUC\VEG_ratio\VEG_ratio.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'15$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data cellinfo1;
set basin_15;
	ID=HUC_8;
	BasinID=huc_8;
	lat_=lat;
	long_=long;
	area=shape_area;
	drop OBJECTID	GRIDCODE	shape_area Shape_Leng	liuyu	HUC_8 lat long;
run;

data cellinfo;
	merge cellinfo1;
	lat=lat_;
	long=long_;
	merge vatio_15;
run;


data cellinfo;
	set cellinfo(keep=ID BasinID lat long VEG_1 VEG_2 VEG_3 VEG_4 VEG_5 VEG_6 VEG_7 VEG_8 VEG_9 VEG_10 VEG_11 VEG_12);

run;

PROC EXPORT DATA= cellinfo 
            OUTFILE= "E:\HUC\Inputs\Inputs_15\cellinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

data HUCAREA;
	set cellinfo1;
	WatershedID=ID;
	Area_m2=area;
	drop basinID lat_ long_ area;
run;
data HUCAREA;
set HUCAREA(keep=ID WatershedID Area_m2);
run;

PROC EXPORT DATA= HUCAREA 
            OUTFILE= "E:\HUC\Inputs\Inputs_15\hucarea.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;



