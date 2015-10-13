Pro Mon2Ann


 ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
 Year_st=1997
 Year_end=2014
 Num_year=Year_end-Year_st+1
 
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
   
    data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
   
   name=make_array(num_year,/string)
   a=0
   for y=Year_st,Year_end,1 do begin
   name[a]='Burn_fraction_25km_'+string(y,format='(i4)')
   a=a+1
   print,y
   endfor
   
    
  ;data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  max1 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,Num_year,type = type0)
  min1 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,Num_year,type = type0)
  mean1=make_array(dims0[2]-dims0[1]+1,dims0[4]+1,Num_year,type = type0)
  sum1=make_array(dims0[2]-dims0[1]+1,dims0[4]+1,Num_year,type = type0)
  b1=make_array(Num_year,type = type0)
  
    
 for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
    for y=0,num_year-1 do begin
    
    ISnull=0
      for i=0, 11 do begin
      
      b1(i)=data0[a,b,i+y*12]
        
        if (data0[a,b,i+y*12] eq 32767 AND ISNULL eq 0) then isnull=1
                         
      endfor
  
   ; max1(a,b)=max(b1)
   ; min1(a,b)=min(b1)
   ; mean1(a,b)=mean(b1)
   sum1(a,b,y)=total(b1)
        if isnull EQ 1 then  sum1(a,b,y)=0 
       
      endfor
    endfor
  endfor
  
  help,sum1
 ; ENVI_ENTER_DATA, max1,Bnames='Max',map_info=map_info
 ; 
 ;ENVI_ENTER_DATA, min1,Bnames='Min',map_info=map_info
 ; ENVI_ENTER_DATA, mean1,Bnames='Mean', map_info=map_info
  ENVI_ENTER_DATA, sum1,Bnames=name, map_info=map_info
  
END