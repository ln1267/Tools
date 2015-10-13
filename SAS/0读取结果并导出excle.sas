
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_300_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W_300��
	2 �߼����Ŀ¼��libname W_300 "E:\HUC\Outputs_union\Outputs_300";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W_300 "E:\HUC\Outputs_union\Outputs_300";

/*���߼����ж�������Ľ���ļ�*/
data W_300.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_300\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_300.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_300.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_300.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_300\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_300.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_300\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_300.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_300.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_300.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_300.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_300\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET RUNOFF FLOWMCMMon;
run;
data W_300.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_300\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
PROC IMPORT OUT= W_300.GEP_VAL 
            DATAFILE= "E:\HUC\gep\GEP_300.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT=GEP_V_A 
            DATAFILE= "E:\HUC\npp\300_npp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
PROC IMPORT OUT=GPP_V_A 
            DATAFILE= "E:\HUC\gpp\300_gpp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


PROC IMPORT OUT= W_300.ET_VAL 
            DATAFILE= "E:\HUC\ET\ET_300.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W_300.ET_VAL_m 
            DATAFILE= "E:\HUC\ET\m\ET_m300.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*��֤���ݶ������*/

/*----------------------------����Ԥ�������-----------------------------------------------*/


/*----------------------------���ݴ���ʼ-----------------------------------------------*/

/*����GEP����*/
DATA W_300.GEP_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W_300.Monthcarbon(drop=reco);
	*where (year < 20012 and year >= 2000);
RUN;
proc sort data=work.GEP_VERIFY out=W_300.GEP_VERIFY;/*����ȡ����GEP���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W_300.GEP_Val out=W_300.GEP_Val;/*����֤�õ�GEP���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_300.GEP_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_300.GEP_VERIFY W_300.Gep_val (keep=GEP_V);
	GEP_V=GEP_V*0.1;
RUN;

DATA W_300.GEP_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.GEP_VERIFY;
	if GEP_V < 0 then delete ;
RUN;

DATA W_300.GEP_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
PROC EXPORT DATA= W_300.GEP_VERIFY 
            OUTFILE= "E:\HUC\Outputs_union\excle\gep\gep_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;
PROC EXPORT DATA= W_300.GEP_VERIFY_G 
            OUTFILE= "E:\HUC\Outputs_union\excle\gep\gep_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g"; 
RUN;

DATA W_300.GEP_VERIFY_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.GEP_VERIFY;
	if year ne 2000 then delete ;
RUN;
DATA W_300.GEP_VERIFY_G_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.GEP_VERIFY_G;
	if year ne 2000 then delete ;
RUN;

PROC EXPORT DATA= W_300.GEP_VERIFY_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\gep\gep_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_2000"; 
RUN;
PROC EXPORT DATA= W_300.GEP_VERIFY_G_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\gep\gep_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep_g_2000"; 
RUN;

/*����ET����*/
DATA W_300.ET_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA ET_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���ET����*/
	set W_300.Monthflow(keep=CELL YEAR MONTH PET AET);
	*where (year <=2006 and year >= 1983);
RUN;
proc sort data=work.ET_VERIFY out=W_300.ET_VERIFY;/*����ȡ����ET���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W_300.ET_Val out=W_300.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_300.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_300.ET_VERIFY W_300.ET_val (keep=ET_V);
RUN;


DATA W_300.ET_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.ET_VERIFY;
	if ET_V < 0 then delete ;
RUN;
data W_300.ET_VERIFY;
 	merge W_300.ET_VERIFY basin;
	by cell;
	drop huc_8;
run;
DATA W_300.et_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.et_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
data  W_300.et_val_m;
	set W_300.et_val_m;
	et_m=et_v;
	keep huc_8 year month et_m;
run;
data W_300.et_val_m_g;
	set W_300.et_val_m;
	if month<4 or month > 10 then delete;
run;
DATA W_300.et_VERIFY_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.et_VERIFY;
	if year ne 2000 then delete ;
RUN;
DATA W_300.et_VERIFY_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	merge W_300.et_VERIFY_2000 W_300.et_val_m;
	keep cell year month aet pet et_v et_m ;
RUN;

DATA W_300.et_VERIFY_G_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_300.et_VERIFY_G;
	if year ne 2000 then delete ;
RUN;

DATA W_300.et_VERIFY_g_2000;/*ɾ����֤���ݲ����ڵ�ֵ*/
	merge W_300.et_VERIFY_g_2000 W_300.et_val_m_g;
	keep cell year month aet pet et_v et_m ;
RUN;

PROC EXPORT DATA= W_300.et_VERIFY 
            OUTFILE= "E:\HUC\Outputs_union\excle\et\et_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et"; 
RUN;
PROC EXPORT DATA= W_300.et_VERIFY_G 
            OUTFILE= "E:\HUC\Outputs_union\excle\et\et_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g"; 
RUN;
PROC EXPORT DATA= W_300.et_VERIFY_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\et\et_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_2000"; 
RUN;
PROC EXPORT DATA= W_300.et_VERIFY_G_2000 
            OUTFILE= "E:\HUC\Outputs_union\excle\et\et_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et_g_2000"; 
RUN;
