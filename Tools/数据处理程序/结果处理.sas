
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_125_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W_125��
	2 �߼����Ŀ¼��libname W_125 "K:\INPUT\Outputs\Outputs_125";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W_125 "K:\INPUT\Outputs\Outputs_125";

/*���߼����ж�������Ľ���ļ�*/
data W_125.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "K:\INPUT\Outputs\Outputs_125\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_125.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_125.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_125.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "K:\INPUT\Outputs\Outputs_125\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_125.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "K:\INPUT\Outputs\Outputs_125\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_125.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_125.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_125.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_125.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "K:\INPUT\Outputs\Outputs_125\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET SUN_ET RUNOFF FLOWMCMMon;
run;
data W_125.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "K:\INPUT\Outputs\Outputs_125\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
/*Monthly validation database*/
PROC IMPORT OUT= W_125.GEP_VAL 
            DATAFILE= "E:\HUC\MGPP\MGPP_125.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
	/*Zhang ET database*/
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
	/*MODIS ET database*/
PROC IMPORT OUT= W_125.MET_VAL 
            DATAFILE= "E:\HUC\MET\MET_125.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*Annual validation database*/
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

/*��֤���ݶ������*/

/*----------------------------����Ԥ�������-----------------------------------------------*/


/*----------------------------���ݴ���ʼ-----------------------------------------------*/
libname W_125 "K:\INPUT\Outputs\Outputs_125";
ods printer pdf file ='K:\INPUT\Outputs\excle\Validation_125.pdf';/*Ϊÿ����������߼���*/

/*����GEP����*/
DATA W_125.GEP_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W_125.Monthcarbon(drop=reco);
	where year = 2000;
RUN;
proc sort data=work.GEP_VERIFY out=W_125.GEP_VERIFY;/*����ȡ����GEP���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W_125.GEP_Val out=W_125.GEP_Val;/*����֤�õ�GEP���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_125.Gep_val;
	set W_125.Gep_val;
	if year ne 2000 then delete;
run;
DATA W_125.GEP_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_125.GEP_VERIFY W_125.Gep_val (keep=GEP_V);
RUN;

DATA W_125.GEP_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.GEP_VERIFY;
	if GEP_V < 0 or NEE < -1000 then delete ;
RUN;

DATA W_125.GEP_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

/*����Zhang ET����*/
DATA W_125.ET_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA ET_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���ET����*/
	set W_125.Monthflow(keep=CELL YEAR MONTH PET AET);
	where year =2000;
RUN;
proc sort data=work.ET_VERIFY out=W_125.ET_VERIFY;/*����ȡ����ET���ݽ�������*/
     by cell YEAR MONTH;
run; 

proc sort data=W_125.ET_Val out=W_125.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_125.ET_val;
	set W_125.ET_val;
	if year ne 2000 then delete;
run;
/*����MODIS ET����*/

proc sort data=W_125.MET_Val out=W_125.MET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 

/*����ET����*/
DATA W_125.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_125.ET_VERIFY W_125.ET_val (keep=ET_V) W_125.MET_val (keep=MET_V);
RUN;

DATA W_125.MET_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.ET_VERIFY;
	if AET <0 or MET_V <0 then delete ;
RUN;

DATA W_125.ET_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.ET_VERIFY;
	if AET <0 or ET_V < 0 or MET_V <0 then delete ;
RUN;

DATA W_125.ET_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;


PROC IMPORT OUT= W_125.basin_125 
            DATAFILE= "E:\HUC\basins\basin_125.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data basin;
	set W_125.basin_125(keep= HUC_8 shape_area);
	cell=HUC_8;
run;

/*---------------------------------��ʼ����GEp����------------------------------*/
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
proc means data=GEP_V noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
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
/*��֤GEP���ݵ�ɢ��ͼ����ع���*/

proc reg data=W_125.Gep_verify;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125��֤���";
run;
quit;
/*��֤GEP���ݵ�ɢ��ͼ����ع���*/
/*
proc reg data=gep_v;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125����������֤���";
	by liuyu;
run;
quit; 
**/
data gep_v_g;
  set gep_v;
  if month < 4 or month > 10 then delete;
run;
/*
proc reg data=gep_v_g;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125��������֤���";
run;
quit; 

proc reg data=gep_v_g;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_125����������������֤���";
	by year;
run;
quit; 
/**
PROC EXPORT DATA= WORK.Gep_v_g_2000 
            OUTFILE= "K:\INPUT\Outputs\excle\gep\gep_A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g_2000"; 
RUN;
*/

	/*��������õ�GEP���ݼ�*/

PROC EXPORT DATA= WORK.Gep_v 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
PROC EXPORT DATA= WORK.Gep_v_g 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g"; 
RUN;

/*------------------------GEP�������---------------------------*/


/*------------------------��ʼ����ET����---------------------------*/
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
proc means data=et_V noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_et(drop=_type_ _freq_) sum=;                                                                                    
run;  
data et_v;
	merge out_et basin val;
	aet=aet/2402628997.4;
	pet=pet/2402628997.4;
	keep month year aet pet et_V et_m;
	if month <1 then delete;
run;

data et_v_g;
	set et_v;
	if month <4 or month >10 then delete;
run;

	/*��������õ�ET���ݼ�*/
PROC EXPORT DATA= WORK.ET_v 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et"; 
RUN;
PROC EXPORT DATA= WORK.ET_v_g 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g"; 
RUN;
/*
PROC EXPORT DATA= WORK.ET_v_g_2000 
            OUTFILE= "K:\INPUT\Outputs\excle\et\et_A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g_2000"; 
RUN;
*/
proc reg data=W_125.Et_verify;
	model aet=et_V;
	plot aet*et_V;
	title "Zhang ET_125��֤���";
run;
quit; 
proc reg data=W_125.MEt_verify;
	model aet=Met_V;
	plot aet*Met_V;
	title "MODIS ET_125��֤���";
run;
quit; 
/*
proc reg data=et_v;
	model aet=et_m;
	plot aet*et_m;
	title "MODIS ET_125��֤���";
run;
quit; 
/*------------------------ET�������---------------------------*/


/*------------------------��ʼ����FLOW����---------------------------*/
proc sort data=W_125.monthflow out=flow_v;
	by year month;
run;

proc means data=flow_v noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_flow(drop=_type_ _freq_) sum=;                                                                                    
run;  
data flow_v;
	merge out_flow val;
	keep year month FLOWMCMMon run_v;
	if month <1 then delete;
run;

data flow_v_g;
	set flow_v;
	if month<4 or month >10 then delete;
run;

proc reg data=flow_v;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW_125��֤���";
run;
quit; 
/*
proc reg data=flow_v_g;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW_125��������֤���";
run;
quit; 
*/
	/*��������õ�FLOW���ݼ�*/
PROC EXPORT DATA= WORK.flow_v 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow"; 
RUN;
PROC EXPORT DATA= WORK.flow_v_g 
            OUTFILE= "K:\INPUT\Outputs\excle\A_125.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow_g"; 
RUN;

/*------------------------FLOW�������---------------------------*/

ods printer close;
