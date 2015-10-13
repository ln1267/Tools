
Libname climate "G:\Data\climate\Monthly_China";

data climate1;/* ??ANNUALFLOW(??????)*/
	infile "G:\Data\climate\Monthly China\SURF_1.TXT"  dlm=',' dsd missover firstobs=2;;
	input Station year month V1 V2 V3 V4 V5 V6 V7 Pre V8 V9 Temp ;
run;

data climate2;/* ??ANNUALFLOW(??????)*/
	infile "G:\Data\climate\Monthly China\SURF_2.TXT"  dlm=',' dsd missover firstobs=2;;
	input Station year month V1 V2 V3 V4 V5 V6 V7 Pre V8 V9 Temp;
run;

data climate3;/* ??ANNUALFLOW(??????)*/
	infile "G:\Data\climate\Monthly China\SURF_3.TXT"  dlm=',' dsd missover firstobs=2;;
	input Station year month V1 V2 V3 V4 V5 V6 V7 Pre V8 V9 Temp ;
run;


data climate4;/* ??ANNUALFLOW(??????)*/
	infile "G:\Data\climate\Monthly China\SURF_4.TXT"  dlm=',' dsd missover firstobs=2;;
	input Station year month V1 V2 V3 V4 V5 V6 V7 Pre V8 V9 Temp;
run;

data climate5;/* ??ANNUALFLOW(??????)*/
	infile "G:\Data\climate\Monthly China\SURF_5.TXT"  dlm=',' dsd missover firstobs=2;;
	input Station year month V1 V2 V3 V4 V5 V6 V7 Pre V8 V9 Temp ;
run;

Data Climate.Climate;

set Climate1  Climate2  Climate3  Climate4  Climate5;

If year<2009 then delete;
Keep Station year month Pre Temp ;
Run;

Data Climate.pre;

set Climate.Climate;
pre=pre/10;
If year>2010 then delete;
Keep Station year month Pre ;
Run;

Data Climate.temp;

set Climate.Climate;
temp=temp/10;
If year>2010 then delete;
Keep Station year month temp ;
Run;


proc transpose data=Climate.pre out=Climate.pre1;                                                                                                     
        var pre  ;                                                                                                       
        by station year;                                                                                                                     
run; 

proc transpose data=Climate.temp out=Climate.temp1;                                                                                                     
        var temp  ;                                                                                                       
        by station year;                                                                                                                     
run; 


Data Climate.station;

set station;
lon=long;
Keep Station lon lat elev ;
Run;


proc sort data=Climate.station out=Climate.station;/*?????GEP??????*/
     by station;
run; 

proc sort data=Climate.temp1 out=Climate.temp1;                                                                                                     
                                                                                                          
        by station year;                                                                                                                     
run; 

proc sort data=Climate.pre1 out=Climate.pre1;                                                                                                     
                                                                                                          
        by station year;                                                                                                                     
run; 



Data Climate.pre2;

merge Climate.station Climate.pre1  ;
by station;
Run;

Data Climate.pre2;

merge Climate.pre1 Climate.station ;
by station;
Run;

Data Climate.pre2;

set Climate.pre2 ;
if lat = "" or year = "" then delete;
if year >2009 then delete;
Run;


PROC EXPORT DATA= Climate.pre2
            OUTFILE= "G:\Data\climate\Monthly_China\monthly_climate.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHLY_pre"; 
RUN;
