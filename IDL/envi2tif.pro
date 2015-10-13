Pro ENVI2tif

  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  out_name = make_array(nb0,/string)
  print,dims0
  a=0
  for y=1981,1960,-1 do begin
  for m=12,1,-1 do begin
  out_name[a] = 'J:/Climate_1961-82/tif/temp_'+string(y,format='(i4.4)')+string(m,format='(i2.2)')+'.tif'
  print,out_name[a]
  a=a+1
  end
  end
  data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,type = type0)
  help,data0
  for i=0, nb0-1 do begin
    data0[*,*] = envi_get_data(fid = fid0,dims = dims0, pos = i)
    
    write_tiff,out_name[i],data0,/float
  endfor
  
  help,data0
 

END