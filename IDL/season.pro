Pro season


 ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
 
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
   
    data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
   
   
  ;data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  max1 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = float)
  min1 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = float)
  mean1=make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = float)
  b1=make_array(nb0,type = type0)
  
 for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  b1(i)=data0[a,b,i]
  
     
  endfor
  
  max1(a,b)=max(b1)
  min1(a,b)=min(b1)
  mean1(a,b)=mean(b1)
  endfor
  endfor
  
  help,max1,min1,mean1
  ENVI_ENTER_DATA, max1,Bnames='Max',map_info=map_info
  
  ENVI_ENTER_DATA, min1,Bnames='Min',map_info=map_info
  ENVI_ENTER_DATA, mean1,Bnames='Mean', map_info=map_info
  
  
END