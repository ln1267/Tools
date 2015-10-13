
/**需更改的内容有
	一般只需将"200"替换为文件名中“Outputs_”和“_n”之间的值；
		eg：Outputs_125_3的文件名则将"200"替换为“200”
	1 逻辑库名称：“libname W_125”
	2 逻辑库的目录：libname W_125 "E:\HUC\Outputs_union\Outputs_125";
	3 读入验证数据在XLXS中的SHEET表名RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------数据预处理开始-----------------------------------------------*/
/*为每个输出创建逻辑库*/
libname W_125 "E:\HUC\Outputs_union\Outputs_125";

/*向逻辑库中读入输出的结果文件*/
data W_125.ANNUALBIO;/* 读入ANNUALBIO（流域年物种丰富度）*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_125.ANNUALCARBON;/* 读入ANNUALCARBON（流域年碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_125.ANNUALFLOW;/* 读入ANNUALFLOW（流域年水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_125.FLOWVOLBYLANDUSE;/* 读入FLOWVOLBYLANDUSE（流域五种主要植被类型（农田，森林，草地，灌丛，无植被）的年径流）*/
	infile "E:\HUC\Outputs_union\Outputs_125\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_125.HUCBIO;/* 读入HUCBIO（流域年均物种丰富度）*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_125.HUCCARBON;/* 读入HUCCARBON（流域年均碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_125.HUCFLOW;/* 读入HUCFLOW（流域年均水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_125.MONTHCARBON;/* 读入MONTHCARBON（流域月碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_125.MONTHFLOW;/* 读入MONTHFLOW（流域月水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_125\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET RUNOFF FLOWMCMMon;
run;
data W_125.RUNOFFBYLANDUSE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "E:\HUC\Outputs_union\Outputs_125\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*模拟结果文件读入结束*/


/*读入验证数据*/
PROC IMPORT OUT= W_125.GEP_VAL 
            DATAFILE= "E:\HUC\gep\GEP_125.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT=GEP_V_A 
            DATAFILE= "E:\HUC\npp\125_npp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
PROC IMPORT OUT=GPP_V_A 
            DATAFILE= "E:\HUC\gpp\125_gpp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


PROC IMPORT OUT= W_125.ET_VAL 
            DATAFILE= "E:\HUC\ET\ET_125.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*验证数据读入结束*/

/*----------------------------数据预处理结束-----------------------------------------------*/


/*----------------------------数据处理开始-----------------------------------------------*/

/*处理GEP数据*/
DATA W_125.GEP_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA GEP_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的GEP数据*/
	set W_125.Monthcarbon(drop=reco);
	*where (year < 20012 and year >= 2000);
RUN;
proc sort data=work.GEP_VERIFY out=W_125.GEP_VERIFY;/*对提取出的GEP数据进行排序*/
     by cell YEAR MONTH;
run; 
proc sort data=W_125.GEP_Val out=W_125.GEP_Val;/*对验证用的GEP数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_125.Gep_val;
	set W_125.Gep_val;
	if year ne 2000 then delete;
run;
DATA W_125.GEP_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W_125.GEP_VERIFY W_125.Gep_val (keep=GEP_V);
	GEP_V=GEP_V*0.1;
RUN;

DATA W_125.GEP_VERIFY;/*删除验证数据不存在的值*/
	set W_125.GEP_VERIFY;
	if GEP_V < 0 then delete ;
RUN;

DATA W_125.GEP_VERIFY_G;/*删除验证数据不存在的值*/
	set W_125.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
