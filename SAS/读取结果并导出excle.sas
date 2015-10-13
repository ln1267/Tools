
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_125_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W_125��
	2 �߼����Ŀ¼��libname W_125 "E:\HUC\Outputs_union\Outputs_125";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W_125 "E:\HUC\Outputs_union\Outputs_125";

/*���߼����ж�������Ľ���ļ�*/
data W_125.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_125.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_125.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_125.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_125\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_125.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_125.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_125.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_125.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_125.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_125\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET RUNOFF FLOWMCMMon;
run;
data W_125.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_125\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
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
/*��֤���ݶ������*/

/*----------------------------����Ԥ�������-----------------------------------------------*/


/*----------------------------���ݴ���ʼ-----------------------------------------------*/

/*����GEP����*/
DATA W_125.GEP_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W_125.Monthcarbon(drop=reco);
	*where (year < 20012 and year >= 2000);
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
	GEP_V=GEP_V*0.1;
RUN;

DATA W_125.GEP_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.GEP_VERIFY;
	if GEP_V < 0 then delete ;
RUN;

DATA W_125.GEP_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_125.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
