;This program is used to transfer NDVI real value (-1 to 1 ) to byte value (0 to 255)

PRO math


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
 
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
;  print,dims0,map_info
;  dims0[1]=000
;  dims0[2]=999
  
  name=make_array(156,/string)
  name1=make_array(156,/string)
  a=0

  for y=2000,2000,1 do begin
  
  	for m=1,23,1 do begin
  	
  	name[a]='NDVI_'+string(y,format='(i4)')+string(m,format='(I2.2)')
  	name1[a]='J:\NDVI\Month_mean\byte\NDVI_MMon_'+string(y,format='(i4)')+string(m,format='(I2.2)')
  	a=a+1
  	endfor
  endfor
  
  print,dims0
  
  ;data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  b2 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = type0)
  b1 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = byte)
 
  for i=0, nb0-1 do begin
    ;data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
   b2[*,*] = envi_get_data(fid = fid0,dims = dims0, pos = i)
   b1= byte((b2+1)*255/2)
  ; help,b1,b2

  ; ENVI_WRITE_ENVI_FILE,b1,OUT_NAME=name1[i],BNAMES=name[i],map_info=map_info 
   print,name1[i]
  endfor
  
  
end
