;trend analysis
PRO trend


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  data0 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  ;
  ;processing
  
  b1= 0
  b2= 0
  b3= 0
  b4= 0
   
  for i=0, nb0-1 do begin
    b1= float(data0[*,*,i])*(i+1)+b1
    b2= float(data0[*,*,i])+b2
    b3= float(i+1+b3)
    b4= float((i+1)^2+b4)
   
  endfor

b5=(b1-(b2*b3/nb0))/(b4-(b3^2/nb0))

ENVI_ENTER_DATA, b5,map_info=map_info

end

;-----------------------------------------------------
;trend v value
PRO trendv


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
  ;
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  data0 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  ;
  ;processing
  
  b1= 0
  b2= 0
  b3= 0
  b4= 0
   
  for i=0, nb0-1 do begin
    b1= data0[*,*,i]+b1
   endfor
  ;
   b1=b1/nb0
  ;
  for i=0, nb0-2 do begin
    b2= float((data0[*,*,i]-data0[*,*,i+1]))^2+b2
  endfor
  ;
  for i=0, nb0-1 do begin
    b3= float((data0[*,*,i]-b1))^2+b3
  endfor
  ;
   b4=b2/b3

ENVI_ENTER_DATA, b4,map_info=map_info

end