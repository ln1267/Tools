Pro WaSSI_readout

; This pro is used for reading and processing outputs documents of WaSSI-C model
  input_dic="J:\Data_WaSSIC\WaSSI_ZA\Outputs_82_12\"
  ;input_dic="J:\Data_WaSSIC\WaSSI_ZA\Outputs_mj\"
 ; 
 input_name1=input_dic+'HUCCARBON.TXT'
 input_name2=input_dic+'HUCFLOW.txt'
 input_name3=input_dic+'ANNUALCARBON.txt'
 input_name4=input_dic+'ANNUALFLOW.txt'
 ;input_name5=input_dic+'MONTHCARBON.txt'
 ;input_name6=input_dic+'MONTHFLOW.txt'
 input_name7=input_dic+'DATA_V_F.txt'

 OpenR , lun1,input_name1,/Get_Lun
 OpenR , lun2,input_name2,/Get_Lun
 OpenR , lun3,input_name3,/Get_Lun
 OpenR , lun4,input_name4,/Get_Lun
 ;OpenR , lun5,input_name5,/Get_Lun
; OpenR , lun6,input_name6,/Get_Lun
 OpenR , lun7,input_name7,/Get_Lun

;Zagunao
samples= 68 
lines= 76     

;;China
;samples=133
;lines= 76

;Minjiang
;samples=157
;lines= 252

year=31
year_start=1982
Year_end=2012
month=12

;Zagunao
      mc = [.0D,.0D,102.51770111,31.92849875]
;
      ps = [1D/100, 1D/100]
      
;Minjiang
;      mc = [.0D,.0D,102.39770111, 33.20849875]
;
;      ps = [1D/100, 1D/100]
;
;China
;      mc = [.0D,.0D,69.8,55.5]
;
;      ps = [1D/2, 1D/2]

      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
;
      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
;;      


;Read HUCCARBON

  
  HUC_GEP=make_array(samples,lines,type = Float)
  HUC_REO=make_array(samples,lines,type = Float)
  HUC_NEE=make_array(samples,lines,type = Float)
 
  vv=""
  Readf,lun1,vv
  print,vv
   
  for a=0, samples-1 do begin
    for b=0, lines-1 do begin
      BasinID=0
      GEP=0.0
      REO=0.0
      NEE=0.0
      Readf,lun1,BasinID,BasinID,GEP,REO,NEE
     ; print,BasinID,BasinID,GEP,REO,NEE
      HUC_GEP[a,b]=GEP
      HUC_REO[a,b]=REO
      HUC_NEE[a,b]=NEE
      ;  Readf,lun1,BasinID,BasinID,HUC_GEP[a,b],HUC_REO[a,b],HUC_NEE[a,b]
     ; print,BasinID,BasinID,HUC_GEP[a,b],HUC_REO[a,b],HUC_NEE[a,b]
    endfor
  endfor
   free_lun,lun1

ENVI_ENTER_DATA,HUC_GEP,Bnames="HUC_GEP",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_REO,Bnames="HUC_REO",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_NEE,Bnames="HUC_NEE",MAP_INFO=map_info

;Read HUCFLOW

;  
  HUC_RAIN=make_array(samples,lines,type = Float)
  HUC_PET=make_array(samples,lines,type = Float)
  HUC_AET=make_array(samples,lines,type = Float)
  HUC_RUNOFF=make_array(samples,lines,type = Float)
  HUC_R_P=make_array(samples,lines,type = Float)
  HUC_ET_P=make_array(samples,lines,type = Float)
 
  vv=""
  Readf,lun2,vv
  print,vv
   
  for a=0, samples-1 do begin
    for b=0, lines-1 do begin
      BasinID=0
      RAIN=0.0
      PET=0.0
      AET=0.0
      RUNOFF=0.0
      R_P=0.0
      ET_P=0.0
      Readf,lun2,BasinID,RAIN,PET,AET,RUNOFF,R_P,ET_P
     ; print,BasinID,RAIN,PET,AET,RUNOFF,R_P,ET_P

      HUC_RAIN[a,b]=RAIN
      HUC_PET[a,b]=PET
      HUC_AET[a,b]=AET
      HUC_RUNOFF[a,b]=RUNOFF
      HUC_R_P[a,b]=R_P
      HUC_ET_P[a,b]=ET_P
      
   
    endfor
  endfor
   free_lun,lun2

ENVI_ENTER_DATA,HUC_RAIN,Bnames="HUC_RAIN",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_PET,Bnames="HUC_PET",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_AET,Bnames="HUC_AET",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_RUNOFF,Bnames="HUC_RUNOFF",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_R_P,Bnames="HUC_R_P",MAP_INFO=map_info
ENVI_ENTER_DATA,HUC_ET_P,Bnames="HUC_ET_P",MAP_INFO=map_info


;Read ANNUALCARBON


  Ann_GEP=make_array(samples,lines,year,type = Float)
  Ann_REO=make_array(samples,lines,year,type = Float)
  Ann_NEE=make_array(samples,lines,year,type = Float)
  Ann_AET=make_array(samples,lines,year,type = Float)
  Ann_PET=make_array(samples,lines,year,type = Float)

  vv=""
  Readf,lun3,vv
  print,vv
  
   
   for a=0, samples-1 do begin
     for b=0, lines-1 do begin
      for y=0, year-1 do begin
      BasinID=0
      GEP=0.0
      REO=0.0
      NEE=0.0
      AET=0.0
      PET=0.0
      Readf,lun3,BasinID,BasinID,GEP,REO,NEE,AET,PET
     ; print,BasinID,string(year_start+y,format='(i4)')+"_Ann_GEP",GEP,REO,NEE,AET,PET
      Ann_GEP[a,b,y]=GEP
      Ann_REO[a,b,y]=REO
      Ann_NEE[a,b,y]=NEE
      Ann_AET[a,b,y]=AET
      Ann_PET[a,b,y]=PET
      endfor
    endfor
  endfor
   free_lun,lun3
   
; Draw annual Carbon results   
    name_GEP=make_array(year,/string)
    name_REO=make_array(year,/string)
    name_NEE=make_array(year,/string)
    name_AET=make_array(year,/string)
    name_PET=make_array(year,/string)
    
   For y=0,year-1 do begin
    
    name_GEP[y]="Ann_GEP_"+string(year_start+y,format='(i4)')
    name_REO[y]="Ann_REO_"+string(year_start+y,format='(i4)')
    name_NEE[y]="Ann_NEE_"+string(year_start+y,format='(i4)')
    name_AET[y]="Ann_AET_"+string(year_start+y,format='(i4)')
    name_PET[y]="Ann_PET_"+string(year_start+y,format='(i4)')

  endfor
  
    ENVI_ENTER_DATA,Ann_GEP,Bnames=name_GEP,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_REO,Bnames=name_REO,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_NEE,Bnames=name_NEE,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_AET,Bnames=name_AET,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_PET,Bnames=name_PET,MAP_INFO=map_info 
     
;Read ANNUALFLOW


  Ann_RAIN=make_array(samples,lines,year,type = Float)
  Ann_SET=make_array(samples,lines,year,type= Float)
  Ann_RUNOFF=make_array(samples,lines,year,type = Float)
  Ann_R_P=make_array(samples,lines,year,type = Float)
  Ann_ET_P=make_array(samples,lines,year,type = Float)
  Ann_Snow=make_array(samples,lines,year,type = Float)
  
  vv=""
  Readf,lun4,vv
  print,vv
  
   
   for a=0, samples-1 do begin
     for b=0, lines-1 do begin
      for y=0, year-1 do begin
      BasinID=0
      v2=0.0
      RAIN=0.0
      SET=0.0
      RUNOFF=0.0 
      R_P=0.0
      ET_P=0.0
      Snow=0.0
      Readf,lun4,BasinID,BasinID,RAIN,v2,v2,SET,RUNOFF,R_P,ET_P,v2,Snow
    ;  print,BasinID,BasinID,RAIN,v2,v2,SET,RUNOFF,R_P,ET_P,v2,Snow

      Ann_RAIN[a,b,y]=RAIN
      Ann_SET[a,b,y]=SET
      Ann_RUNOFF[a,b,y]=RUNOFF
      Ann_R_P[a,b,y]=R_P
      Ann_ET_P[a,b,y]=ET_P
      Ann_Snow[a,b,y]=Snow

      endfor
    endfor
  endfor
   free_lun,lun4
   help,Ann_Rain
   
 ;  Draw annual flow results   
    name_RAIN=make_array(year,/string)
    name_SET=make_array(year,/string)
    name_RUNOFF=make_array(year,/string)
    name_R_P=make_array(year,/string)
    name_ET_P=make_array(year,/string)
    name_Snow=make_array(year,/string)
   
   
   For y=0,year-1 do begin
    
    name_RAIN[y]="Ann_RAIN_"+string(year_start+y,format='(i4)')
    name_SET[y]="Ann_SET_"+string(year_start+y,format='(i4)')
    name_RUNOFF[y]="Ann_RUNOFF_"+string(year_start+y,format='(i4)')
    name_R_P[y]="Ann_R_P_"+string(year_start+y,format='(i4)')
    name_ET_P[y]="Ann_ET_P_"+string(year_start+y,format='(i4)')
    name_Snow[y]="Ann_Snow_"+string(year_start+y,format='(i4)')

  endfor
  
    ENVI_ENTER_DATA,Ann_RAIN,Bnames=name_RAIN,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_SET,Bnames=name_SET,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_RUNOFF,Bnames=name_RUNOFF,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_R_P,Bnames=name_R_P,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_ET_P,Bnames=name_ET_P,MAP_INFO=map_info 
    ENVI_ENTER_DATA,Ann_Snow,Bnames=name_Snow,MAP_INFO=map_info 

;Read mean monthly data


  MMon_Baseflow=make_array(samples,lines,12,type = Float)
  MMon_RUNOFF=make_array(samples,lines,12,type = Float)

  
  vv=""
  Readf,lun7,vv
  print,vv
  
   
   for a=0, samples-1 do begin
     for b=0, lines-1 do begin
      for m=0, 12-1 do begin
      BasinID=0
      BasinID1=0
      v2=0.0

      RUNOFF=0.0 
      baseflow=0.0

      Readf,lun7,BasinID,BasinID1,RUNOFF,baseflow
    ;  print,BasinID,BasinID1,RUNOFF,baseflow

      MMon_RUNOFF[a,b,m]=RUNOFF
      MMon_Baseflow[a,b,m]=baseflow


      endfor
    endfor
  endfor
   free_lun,lun7
   help,MMon_RUNOFF
   
 ;  Draw Mean Monthly flow results   
    name_MMon_RUNOFF=make_array(12,/string)
    name_MMon_baseflow=make_array(12,/string)
  
   
   For m=0,12-1 do begin
    
    name_MMon_RUNOFF[m]="MMon_RUNOFF"+string(1+m,format='(i2)')
    name_MMon_baseflow[m]="MMon_baseflow"+string(1+m,format='(i2)')
    
  endfor
  
    ENVI_ENTER_DATA,MMon_RUNOFF,Bnames=name_MMon_RUNOFF,MAP_INFO=map_info
    ENVI_ENTER_DATA,MMon_baseflow,Bnames=name_MMon_baseflow,MAP_INFO=map_info
   

END