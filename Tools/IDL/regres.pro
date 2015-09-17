PRO Regres


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  
  ; For WUE cacluation
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
   ENVI_SELECT, fid=fid1,dims=dims1,pos=po1
    map_info = envi_get_map_info(fid=fid1)
  ENVI_FILE_QUERY, fid1, dims=dims1, nb=nb1,data_type = type1
  data02 = make_array(dims1[2]+1,dims1[4]+1,nb1,type = float)
  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
  data0= make_array(dims0[2]+1,dims0[4]+1,type = float)
  data1 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  data2 = make_array(dims0[2]+1,dims0[4]+1,type = float)
    for i=0, nb0-1 do begin
    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
    data02[*,*,i] = envi_get_data(fid = fid1,dims = dims1, pos = i)
    
  endfor
  
; caculating by each pixel  
  b1=make_array(nb0,type = type0)
  b2=make_array(nb0,type = type0)
  b3=0
  b4=0
  sl=0
  
  for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  b1(i)=float(data01[a,b,i])
  
  b2(i)=float(data02[a,b,i])
     
  endfor
  
  data0(a,b)=regress(b1,b2, SIGMA=sigma, CONST=const, $
     MEASURE_ERRORS=measure_errors)
  data1(a,b)=const
  data2(a,b)=CORRELATE(b1,b2)
  endfor
  endfor
  help,data0,data1,data2
  ENVI_ENTER_DATA, data0,Bnames='regress',map_info=map_info
  
  ENVI_ENTER_DATA, data1,Bnames='constant',map_info=map_info
  ENVI_ENTER_DATA, data2,Bnames='Corelate', map_info=map_info

end

