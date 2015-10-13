libname W_125 "E:\HUC\Outputs_union\Outputs_125";
ods printer pdf file ='E:\HUC\Outputs_union\excle\Validation_125.pdf';/*为每个输出创建逻辑库*/

PROC IMPORT OUT= W_125.basin_125 
            DATAFILE= "E:\HUC\basins\basin_125.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data basin;
	set W_125.basin_125(keep= HUC_8 shape_area);
	cell=HUC_8;
run;
data W_125.GEP_VERIFY;
 	merge W_125.GEP_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data gep_v;
	SET W_125.GEP_VERIFY;
	gep=gep*shape_area;
	
	keep cell year month gep  ;
	if month <= 0 then delete;
run;
proc sort data=gep_v;
	by year month ;
run;
proc means data=GEP_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) sum=;                                                                                    
run;  


PROC IMPORT OUT= val 
            DATAFILE= "E:\HUC\val_data_za.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="gep$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;


data gep_v;
	merge out_gep basin val;
	gep=gep/2402628997.4;

	keep year month  gep gep_V;
	if month <1 then delete;
run;
/**
PROC EXPORT DATA= WORK.Gep_v 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_125_v.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
/*验证GEP数据的散点图及其回归线*/

proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125验证结果";
run;
quit;
/*验证GEP数据的散点图及其回归线*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125三大流域验证结果";
	by liuyu;
run;
quit; 
**/
data gep_v_g;
  set gep_v;
  if month < 4 or month > 10 then delete;
run;
data gep_v_g_2000;
	set gep_v_g;
	if year ne 2000 then delete;
run;
proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125生长季验证结果";
run;
quit; 
proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125三大流域生长季验证结果";
	by year;
run;
quit; 
/**
PROC EXPORT DATA= WORK.Gep_v_g_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\gep\gep_A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g_2000"; 
RUN;
*/
PROC EXPORT DATA= WORK.Gep_v 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
PROC EXPORT DATA= WORK.Gep_v_g 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g"; 
RUN;



DATA W_125.ET_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA ET_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的ET数据*/
	set W_125.Monthflow(keep=CELL YEAR MONTH PET AET);
	*where (year <=2006 and year >= 1983);
RUN;
proc sort data=work.ET_VERIFY out=W_125.ET_VERIFY;/*对提取出的ET数据进行排序*/
     by cell YEAR MONTH;
run; 


data W_125.et_VERIFY;
 	merge W_125.et_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data et_v;
	SET W_125.et_VERIFY;
	aet=aet*shape_area;
	pet=pet*shape_area;
	keep cell year month aet pet ;
	if month <= 0 then delete;
run;
proc sort data=et_v;
	by year month ;

run;
proc means data=et_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_et(drop=_type_ _freq_) sum=;                                                                                    
run;  
data et_v;
	merge out_et basin val;

	aet=aet/2402628997.4;
	pet=pet/2402628997.4;
	keep month year aet pet et_V et_m;
run;

data et_v_g;
	set et_v;
	if month <4 or month >10 then delete;

run;
data ET_v_g_2000;
	set ET_v_g;
	if year ne 2000 then delete;
run;
PROC EXPORT DATA= WORK.ET_v 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et"; 
RUN;
PROC EXPORT DATA= WORK.ET_v_g 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g"; 
RUN;
/*
PROC EXPORT DATA= WORK.ET_v_g_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\et\et_A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g_2000"; 
RUN;
*/
proc reg data=et_v_g;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "AET_125验证结果";
run;
quit; 


proc sort data=W_125.monthflow out=flow_v;
	by year month;
run;

proc means data=flow_v noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_flow(drop=_type_ _freq_) sum=;                                                                                    
run;  
data flow_v;
	merge out_flow val;
	keep year month FLOWMCMMon run_v;
run;

data flow_v_g;
	set flow_v;
	if month<4 or month >10 then delete;
run;
proc reg data=flow_v;/*验证et数据的散点图及其回归线*/
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW_125验证结果";
run;
quit; 
proc reg data=flow_v_g;/*验证et数据的散点图及其回归线*/
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW_125生长季验证结果";
run;
quit; 

PROC EXPORT DATA= WORK.flow_v 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow"; 
RUN;
PROC EXPORT DATA= WORK.flow_v_g 
            OUTFILE= "E:\HUC\Outputs_union\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow_g"; 
RUN;
ods printer close;
