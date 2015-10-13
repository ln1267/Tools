Pro outnamelist

openw,lun1,'M:/AU/NDVI/NDVI-new/ndvilist.txt',/get_lun

for y=2000,2012 do begin
    
    for m=1,12 do begin
      
      name1='K:/AU/NDVI/NDVI-new/byte/NDVI_'+string(y,format='(i4)')+string(m,format='(I2.2)')
      printf,lun1, name1
      
    endfor
  
endfor

Free_lun,lun1

end