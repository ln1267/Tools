Pro MeanNDVI

; This pro is used for calculate the mean NDVI for classify rengong and tianran


ENVI_SELECT, fid=fid0,dims=dims0,pos=pos

 
  ;data file
    
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
  print,dims0
  
  data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  
  help,data0
  
  n_month=23
  n_year=7
MMon_NDVI=make_array(dims0[2]-dims0[1]+1,dims0[4]+1,n_month,type = Float)
 name=make_array(n_month,/string)

    for m=0,n_month-1 do begin
    
    name[m]='NDVI_MJ_Mmon_250m_'+string(m+1,format='(I2.2)')
    print,name[m]
    endfor


for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
  
    
    
    for MONTH=0,n_month-1 do begin
      i=0+month   ; the line where the first NDVI data was in
      NDVI=0.0
      for YEAR=0,n_year-1 do begin
      
      NDVI=NDVI+data0[a,b,i+year*n_month]
   
      endfor 
      MMon_NDVI[a,b,month]= NDVI/n_year
        
    endfor

  endfor
endfor
help,MMon_NDVI
 ENVI_ENTER_DATA,MMon_NDVI,BNAMES=name
 
 
END
