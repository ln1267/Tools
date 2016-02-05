Pro LCC


  ; Select data
  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos1
  ENVI_SELECT, fid=fid1,dims=dims1,pos=pos2
  
  ; read map info
  map_info = envi_get_map_info(fid=fid0)
  map_info = envi_get_map_info(fid=fid1)
  
  ; read file info
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  ENVI_FILE_QUERY, fid1, dims=dims1, nb=nb1,data_type = type1
  
  ;make arrays for temporary files
  data01= make_array(dims0[2]+1,dims0[4]+1,type = type0)
  data02= make_array(dims1[2]+1,dims1[4]+1,type = type1)
  data03= make_array(dims0[2]+1,dims0[4]+1,type = type0)
  data04= make_array(dims0[2]+1,dims0[4]+1,type = type0)
  data06= make_array(dims0[2]+1,dims0[4]+1,type = string)
  
  ; read image data
  data01[*,*] = envi_get_data(fid = fid0,dims = dims0,pos=0)
  data02[*,*] = envi_get_data(fid = fid1,dims = dims1,pos=0)
  
  ; 
  for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin

  if(data01[a,b] eq data02[a,b]) then begin
    data03[a,b]=0
    data04[a,b]=0
  endif else begin 
    data03[a,b]=data01[a,b]
    data04[a,b]=data02[a,b]
    data06[a,b]=string(data01[a,b])+'->'+string(data02[a,b])
    print,data03,data04,data06
  endelse
  
  endfor
  endfor
  ENVI_ENTER_DATA, data04,map_info=map_info


END