PROC IMPORT OUT= WORK.sub_all 
            DATATABLE= "sub" 
            DBMS=ACCESS REPLACE;
     DATABASE="H:\SWAT\new\all\Scenarios\ann\TablesOut\SWATOutput.mdb"; 
     SCANMEMO=YES;
     USEDATE=NO;
     SCANTIME=YES;
RUN;
