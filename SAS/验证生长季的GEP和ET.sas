libname W_300 "E:\HUC\Outputs_union\Outputs_300";
ods printer pdf file ='E:\HUC\Outputs_union\Validation_g\Validation_300.pdf';/*为每个输出创建逻辑库*/

data basin;
	set W_300.basin_300(keep=liuyu HUC_8 shape_area);
	cell=HUC_8;
run;
data W_300.GEP_VERIFY;
 	merge W_300.GEP_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data gep_v;
	SET W_300.GEP_VERIFY;
	gep=gep*shape_area;
	
	keep cell month gep  liuyu;
	if month <= 0 then delete;
run;
proc sort data=gep_v;
	by month liuyu;
run;
proc means data=GEP_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by month liuyu;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) sum=;                                                                                    
run;  

proc import out=basins_3
	datafile="E:\HUC\basins\basin_20000.dbf"
	dbms=dbf replace;
	run;
proc sort data=basins_3;
	by liuyu;
	run;
PROC IMPORT OUT= val 
            DATAFILE= "E:\HUC\val_data.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="gep$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
proc sort data=out_gep;
	by liuyu;
	run;
proc sort data=val;
	by liuyu;
	run;

data gep_v;
	merge out_gep basins_3 val;
	by liuyu;
	gep=gep/shape_area;
	gep_v=gep_V*0.1;
	keep month liuyu gep gep_V;
run;
/**
PROC EXPORT DATA= WORK.Gep_v 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_300_v.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
/*验证GEP数据的散点图及其回归线*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_300验证结果";
run;
quit;
/*验证GEP数据的散点图及其回归线*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_300三大流域验证结果";
	by liuyu;
run;
quit; 
**/
data gep_v_g;
  set gep_v;
  if month < 4 or month > 10 then delete;
run;

proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_300生长季验证结果";
run;
quit; 
proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_300三大流域生长季验证结果";
	by liuyu;
run;
quit; 
PROC EXPORT DATA= WORK.Gep_v_g 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_300_g.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;



data W_300.et_VERIFY;
 	merge W_300.et_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data et_v;
	SET W_300.et_VERIFY;
	aet=aet*shape_area;
	pet=pet*shape_area;
	keep cell month aet pet liuyu;
	if month <= 0 then delete;
run;
proc sort data=et_v;
	by month liuyu;

run;
proc means data=et_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by month liuyu;                                                                                                                   
        output out=out_et(drop=_type_ _freq_) sum=;                                                                                    
run;  


proc sort data=out_et;
	by liuyu;
	run;

data et_v;
	merge out_et basins_3 val;
	by liuyu;
	aet=aet/shape_area;
	pet=pet/shape_area;
	keep month liuyu aet pet et_V;
run;

data et_v_g;
	set et_v;
	if month <4 or month >10 then delete;

run;
PROC EXPORT DATA= WORK.ET_v_g 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\ET_300_g.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
proc reg data=et_v_g;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "AET_300验证结果";
run;
quit; 
proc reg data=et_v_g;/*验证et数据的散点图及其回归线*/
	model pet=et_V;
	plot pet*et_V;
	title "PET_300验证结果";
run;
quit; 
proc reg data=et_v;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "AET_300三大流域验证结果";
	by liuyu;
run;
quit;
proc reg data=et_v;/*验证et数据的散点图及其回归线*/
	model pet=et_V;
	plot pet*et_V;
	title "PET_300三大流域验证结果";
	by liuyu;
run;
quit;  
ods printer close;
