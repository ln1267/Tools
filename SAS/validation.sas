libname W_80 "E:\HUC\Outputs_union\Outputs_80";
ods printer pdf file ='E:\HUC\Outputs_union\Validation_1\Validation_80.pdf';/*为每个输出创建逻辑库*/
data gep_v_huc;
	set W_80.GEP_VERIFY;
	gep_v=gep_v;
	if month <5 or month >10 then delete;
run;
PROC EXPORT DATA= WORK.gep_v_huc 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_80_huc.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
proc reg data=gep_v_huc;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80生长季验证结果";
run;
quit; 
proc sort data=gep_v_huc;
	by liuyu;
run;
proc reg data=gep_v_huc;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80三大流域生长季验证结果";
	by liuyu;
run;
quit;
data basin;
	set W_80.basin_80(keep=liuyu HUC_8 shape_area);
	cell=HUC_8;
run;
data W_80.GEP_VERIFY;
 	merge W_80.GEP_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data gep_v;
	SET W_80.GEP_VERIFY;
	gep=gep*shape_area;
	gep_v=gep_v*shape_area;
	keep cell month gep gep_v liuyu;
run;
proc sort data=gep_v;
	by month liuyu;
run;
proc means data=GEP_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by month liuyu;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) sum=;                                                                                    
run;  
data out_GEP;
	set out_gep;
	if month <= 0 then delete;
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
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_80_v.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
/*验证GEP数据的散点图及其回归线*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80验证结果";
run;
quit;
/*验证GEP数据的散点图及其回归线*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80三大流域验证结果";
	by liuyu;
run;
quit; 
**/
data gep_v_g;
  set gep_v;
  if month <5 or month >10 then delete;
run;

proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80生长季验证结果";
run;
quit; 
proc reg data=gep_v_g;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_80三大流域生长季验证结果";
	by liuyu;
run;
quit; 
PROC EXPORT DATA= WORK.Gep_v_g 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\gep_80_g.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;

data W_80.et_VERIFY;
 	merge W_80.et_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*validation the sum of three basins*/
data et_v;
	SET W_80.et_VERIFY;
	aet=aet*shape_area;
	et_v=et_v*shape_area;
	keep cell month aet et_v liuyu;
run;
proc sort data=et_v;
	by month liuyu;
run;
proc means data=et_V noprint; /*分别求算每个流域的均值*/                                                                                             
        by month liuyu;                                                                                                                   
        output out=out_et(drop=_type_ _freq_) sum=;                                                                                    
run;  
data out_et;
	set out_et;
	if month <= 0 then delete;
	run;


proc sort data=out_et;
	by liuyu;
	run;

data et_v;
	merge out_et basins_3 val;
	by liuyu;
	aet=aet/shape_area;

	keep month liuyu aet et_V;
run;
PROC EXPORT DATA= WORK.et_v 
            OUTFILE= "C:\Users\LN1267\Desktop\xiaolv\et_80_v.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="et"; 
RUN;
proc reg data=et_v;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "et_80验证结果";
run;
quit; 
proc reg data=et_v;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "et_80三大流域验证结果";
	by liuyu;
run;
quit; 

data et_v_g;
	set et_v;
	if month <5 or month >10 then delete;

run;
proc reg data=et_v_g;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "et_80验证结果";
run;
quit; 
proc reg data=et_v;/*验证et数据的散点图及其回归线*/
	model aet=et_V;
	plot aet*et_V;
	title "et_80三大流域验证结果";
	by liuyu;
run;
quit; 
ods printer close;
