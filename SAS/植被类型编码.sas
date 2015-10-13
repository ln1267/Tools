PROC IMPORT OUT= WORK.VEG_15 
            DATAFILE= "E:\HUC\VEG\VEG_15.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data VEG_151;
set veg_15(keep=HUC_8 VEG_code Shape_Area);
run;
PROC EXPORT DATA= WORK.VEG_15 
            OUTFILE= "E:\HUC\VEG_ratio\VEG_ratio_15.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="sheet1"; 
RUN;
PROC IMPORT OUT= WORK.VEG_20 
            DATAFILE= "E:\HUC\VEG\VEG_20.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data VEG_20;
set veg_20(keep=HUC_8 VEG_code Shape_Area);
run;
PROC EXPORT DATA= WORK.VEG_20 
            OUTFILE= "E:\HUC\VEG_ratio\VEG_ratio_20.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="sheet1"; 
RUN;
