Pro WaSSI_readout_month

; This pro is used for reading and processing outputs documents of WaSSI-C model
  input_dic="M:\CLIMATE_RESULTS\C6\"
  ;input_dic="J:\Data_WaSSIC\WaSSI_ZA\Outputs_mj\"
 ; 
; input_name5=input_dic+'MONTHCARBON.txt'
; input_name6=input_dic+'MONTHFLOW.txt'
 
; OpenR , lun5,input_name5,/Get_Lun
; OpenR , lun6,input_name6,/Get_Lun

;Zagunao
samples=182
lines=260     

;;China
;samples=133
;lines= 76

;Minjiang
;samples=157
;lines= 252

year=13
year_start=2000
Year_end=2012
month=12

;Zagunao
      mc = [.0D,.0D,102.362,33.185]
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


;Read ANNUALCARBON


  Ann_GEP=make_array(samples,lines,year,type = Float)
  Ann_REO=make_array(samples,lines,year,type = Float)
  Ann_NEE=make_array(samples,lines,year,type = Float)
 

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

      Readf,lun3,BasinID,BasinID,GEP,REO,NEE
     ; print,BasinID,string(year_start+y,format='(i4)')+"_Ann_GEP",GEP,REO,NEE,AET,PET
      Ann_GEP[a,b,y]=GEP
      Ann_REO[a,b,y]=REO
      Ann_NEE[a,b,y]=NEE

      endfor
    endfor
  endfor
  free_lun,lun3
  
; Draw annual Carbon results   
    name_GEP=make_array(year,/string)
    name_REO=make_array(year,/string)
    name_NEE=make_array(year,/string)

    
   For y=0,year-1 do begin
    
    name_GEP[y]="Ann_GEP_"+string(year_start+y,format='(i4)')
    name_REO[y]="Ann_REO_"+string(year_start+y,format='(i4)')
    name_NEE[y]="Ann_NEE_"+string(year_start+y,format='(i4)')
   

  endfor
  
    ENVI_ENTER_DATA,Ann_GEP,Bnames=name_GEP,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_REO,Bnames=name_REO,MAP_INFO=map_info
    ENVI_ENTER_DATA,Ann_NEE,Bnames=name_NEE,MAP_INFO=map_info

;-------------------  
;
;   
;Read MONTHFLOW

  Mon_RAIN=make_array(samples,lines,year,12,type = Float)
  Mon_TEMP=make_array(samples,lines,year,12,type= Float)
  Mon_SMC=make_array(samples,lines,year,12,type= Float)
  Mon_Snow=make_array(samples,lines,year,12,type= Float)
  Mon_PET=make_array(samples,lines,year,12,type= Float)
  Mon_AET=make_array(samples,lines,year,12,type= Float)
  Mon_SET=make_array(samples,lines,year,12,type= Float)
  Mon_RUNOFF=make_array(samples,lines,year,12,type= Float)
  
  vv=""
  Readf,lun4,vv
  print,vv
     
   for a=0, samples-1 do begin
     for b=0, lines-1 do begin
      for y=0, year-1 do begin
        for m=0, 12-1 do begin
        BasinID=0
        v2=0.0
        RAIN=0.0
        SET=0.0
        RUNOFF=0.0 
        TEMP=0.0
        SMC=0.0
        Snow=0.0
        AET=0.0
        PET=0.0
        
        Readf,lun4,BasinID,BasinID,BasinID,RAIN,TEMP,SMC,Snow,PET,AET,SET,RUNOFF
  ;      print,BasinID,BasinID,RAIN,v2,v2,SET,RUNOFF,R_P,ET_P,v2,Snow
  
        Mon_RAIN[a,b,y,m]=RAIN
        Mon_SET[a,b,y,m]=SET
        Mon_RUNOFF[a,b,y,m]=RUNOFF
        Mon_TEMP[a,b,y,m]=TEMP
        Mon_SMC[a,b,y,m]=SMC
        Mon_Snow[a,b,y,m]=Snow
        Mon_AET[a,b,y,m]=AET
        Mon_PET[a,b,y,m]=PET
        endfor
      endfor
    endfor
  endfor
  free_lun,lun4
  help,Mon_Rain
   
 ;  Draw Monual flow results   
  name_RAIN=make_array(year,12,/string)
  name_TEMP=make_array(year,12,/string)
  name_SMC=make_array(year,12,/string)
  name_Snow=make_array(year,12,/string)
  name_PET=make_array(year,12,/string)
  name_AET=make_array(year,12,/string)
  name_SET=make_array(year,12,/string)
  name_RUNOFF=make_array(year,12,/string)
   
   For y=0,year-1 do begin
      for m,12-1 do begin
      name_RAIN[y,m]="Mon_RAIN_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_SET[y,m]="Mon_SET_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_RUNOFF[y,m]="Mon_RUNOFF_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_TEMP[y,m]="Mon_TEMP_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_SMC[y,m]="Mon_SMC_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_Snow[y,m]="Mon_Snow_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_AET[y,m]="Mon_AET_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
      name_PET[y,m]="Mon_PET_"+string(year_start+y,format='(i4)')+string(m+1,format='(i2)')
    endfor
  endfor
  for 
    ENVI_ENTER_DATA,Mon_RAIN,Bnames=name_RAIN,MAP_INFO=map_info
    ENVI_ENTER_DATA,Mon_SET,Bnames=name_SET,MAP_INFO=map_info
    ENVI_ENTER_DATA,Mon_RUNOFF,Bnames=name_RUNOFF,MAP_INFO=map_info
    ENVI_ENTER_DATA,Mon_R_P,Bnames=name_R_P,MAP_INFO=map_info
    ENVI_ENTER_DATA,Mon_ET_P,Bnames=name_ET_P,MAP_INFO=map_info 
    ENVI_ENTER_DATA,Mon_Snow,Bnames=name_Snow,MAP_INFO=map_info 
    ENVI_ENTER_DATA,Mon_AET,Bnames=name_AET,MAP_INFO=map_info
    ENVI_ENTER_DATA,Mon_PET,Bnames=name_PET,MAP_INFO=map_info 
;-------------------
;



END