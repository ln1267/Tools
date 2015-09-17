PRO data

  ; For WUE cacluation
  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
    map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
   ENVI_SELECT, fid=fid1,dims=dims1,pos=po1
    map_info = envi_get_map_info(fid=fid1)
   ENVI_FILE_QUERY, fid1, dims=dims1, nb=nb1,data_type = type1
  
  ENVI_SELECT, fid=fid3,dims=dims3,pos=po3
    map_info = envi_get_map_info(fid=fid3)
   ENVI_FILE_QUERY, fid3, dims=dims3, nb=nb3,data_type = type3
  
  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
  data02 = make_array(dims1[2]+1,dims1[4]+1,nb1,type = float)
  data03 = make_array(dims3[2]+1,dims3[4]+1,nb3,type = type3)
  
    
    for i=0, nb0-1 do begin
    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
    data02[*,*,i] = envi_get_data(fid = fid1,dims = dims1, pos = i)
    endfor
  
  for i=0, nb3-1 do begin
    data03[*,*,i] = envi_get_data(fid = fid3,dims = dims3, pos = i)
    endfor
  
  OpenW , lun11,'H:/NandL/data.txt',/Get_Lun
  
  Printf,lun11,'X',',','Y',',','Year',',','Month',',','Type',',','NDVI',',','LAI'
  
; caculating by each pixel  

  
  for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   n=0
  for y=0, 3 do begin
  
  for m=0, 11 do begin
  
  
  Printf,lun11,a+1,',',b+1,',',Y+2001,',',m+1,',',data03[a,b,y],',',data01[a,b,n]*0.004-0.1,',',data02[a,b,n]*0.1

  n=n+1   
  endfor
  endfor
 
  endfor
  endfor
  help,data01,data02,data03

free_lun,lun11
end

