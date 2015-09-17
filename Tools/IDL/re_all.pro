Pro RE_all



; For two variables regress cacluation
;X
  
  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
    map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
;Y
   ENVI_SELECT, fid=fid1,dims=dims1,pos=po1
    map_info = envi_get_map_info(fid=fid1)
  ENVI_FILE_QUERY, fid1, dims=dims1, nb=nb1,data_type = type1
  
;define array for images
  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
  data02 = make_array(dims1[2]+1,dims1[4]+1,nb1,type = float)
  
;else array
  data0= make_array(dims0[2]+1,dims0[4]+1,type = float)
  data1 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  data2 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  data3 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  
;get data from images
    for i=0, nb0-1 do begin
    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
    data02[*,*,i] = envi_get_data(fid = fid1,dims = dims1, pos = i)
    endfor
    
  
  num=long(0)
  help,num  
    for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
    IF ((data01[a,b,i] GE -0.100 and data01[a,b,i] LE 0.100) and (data02[a,b,i] LE 1 and data02[a,b,i] GE -1) )then begin
    
     num=num+1
     ENDIF ELSE begin
     
     ENDELSE
   
     endfor
     endfor
     endfor
  help,num  
    
    
  
; caculating by each pixel  
  x1=make_array(num,type = type0)
  y1=make_array(num,type = type0)
  
  n=long(0)
  OpenW , lun11,'d:/data.txt',/Get_Lun
 
  for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  IF  ((data01[a,b,i] GE -0.100 and data01[a,b,i] LE 0.100) and (data02[a,b,i] LE 1.000 and data02[a,b,i] GE -1.000) )$
   then begin
  
  x1(n)=float(data01[a,b,i])
  
  y1(n)=float(data02[a,b,i])
  n=n+1
   PrintF, lun11,a,b,i,' ',float(data01[a,b,i]),' ',float(data02[a,b,i])
  ENDIF ELSE begin
     
     ENDELSE
     
  endfor
  endfor
  endfor
  
  help,x1,y1
    ;slop
  data=regress(x1,y1, SIGMA=sigma, CONST=const,CORRELATION=Cor_r)
  ;constant
  data1=const
  ;R
  data2=Cor_r

  help,data,data1,data2
 
  OpenW , lun11,'d:/0.txt',/Get_Lun
  PrintF, lun11, x1
   OpenW , lun12,'d:/1.txt',/Get_Lun
  PrintF, lun12, y1
  print,data,data1,data2
  ;ENVI_ENTER_DATA, data0,Bnames='Slop',map_info=map_info
  ;ENVI_ENTER_DATA, data1,Bnames='Contant',map_info=map_info
;  ENVI_ENTER_DATA, data2,Bnames='R', map_info=map_info
;  ENVI_ENTER_DATA, data3,Bnames='P', map_info=map_info
END