libname Climate "E:\Climate\climate day\V3\V3day";


DATA temp; /*?Monthcarbon???????2000-20011??GEP??*/
	set Climate.tem;
	if QA_mean > 0 then delete;
	if tem_mean >800 then delete;
RUN;


proc sort data=temp out=Climate.temp_real;/*?????GEP??????*/
     by Station YEAR MONTH;
run; 

proc means data=Climate.temp_real noprint; /*???????????*/                                                                                             
        by Station year month ;                                                                                                                   
        output out=Climate.temp_Month_real(drop=_type_ _freq_) mean=;                                                                                    
run;  



DATA Pre; /*?Monthcarbon???????2000-20011??GEP??*/
	set Climate.pre;
	if QA > 0 then delete;
	if Pre >30000 then delete;
RUN;


proc sort data=pre out=Climate.pre_real;/*?????GEP??????*/
     by Station YEAR MONTH;
run; 

proc means data=Climate.pre_real noprint; /*???????????*/                                                                                             
        by Station year month ;                                                                                                                   
        output out=Climate.pre_Month_sum_real(drop=_type_ _freq_) sum=;                                                                                    
run;  
proc means data=Climate.pre_real noprint; /*???????????*/                                                                                             
        by Station year month ;                                                                                                                   
        output out=Climate.pre_Month_mean_real(drop=_type_ _freq_) mean=;                                                                                    
run;  
PROC EXPORT DATA= Climate.pre_Month_mean_real
            OUTFILE= "C:\Users\ning\Desktop\pre_month.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="pre_mean"; 
RUN;
PROC EXPORT DATA= Climate.pre_Month_sum_real
            OUTFILE= "C:\Users\ning\Desktop\pre_month.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="pre_sum"; 
RUN;
