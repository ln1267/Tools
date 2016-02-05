;This program is used to transfer NDVI real value (-1 to 1 ) to byte value (0 to 255)

PRO class


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
 
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
;  print,dims0,map_info
;  dims0[1]=000
;  dims0[2]=999
  
  name=make_array(12,/string)
  name1=make_array(12,/string)
  a=0

  for y=2000,2011,1 do begin
  
    ;for m=1,1,-16 do begin
    
    name[a]='MOD_DSI_'+string(y,format='(i4)')
   ; name1[a]='G:\CN\Minj\NDVI\byte\NDVI_MJ_QA_'+string(y,format='(i4)')+string(m,format='(I3.3)')
    a=a+1
   ; endfor
  endfor
      
      data0 = make_array(dims0[2]-dims0[1]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
   
  
  for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  if (data0[a,b,i] GE 1.2 and data0[a,b,i] lE 10 ) then begin
  
   
   data0[a,b,i]=10
   endif else begin
   
   if (data0[a,b,i] GE 1 and data0[a,b,i] lt 1.2 ) then begin
   
    data0[a,b,i]=5
   
    endif else begin
  
  if (data0[a,b,i] GE -1.2 and data0[a,b,i] lE -1 ) then begin
  
  data0[a,b,i]=-5  
  endif else begin
  if (data0[a,b,i] GE -10 and data0[a,b,i] lE -1.2 ) then begin
  
  data0[a,b,i]=-10  
  endif else begin
  
  data0[a,b,i]=0
  
  
  
  endelse
  endelse
  endelse
  endelse
  
  
  
  endfor
  
   endfor
  endfor
  
  

    ENVI_ENTER_DATA,data0,BNAMES=name,map_info=map_info 
   print,name

  
  
end
