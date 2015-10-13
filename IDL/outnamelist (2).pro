Pro outnamelist

openw,lun1,'G:/CN/Minj/NDVI/ndvilist_QA.txt',/get_lun

printf,lun1,'322'

for y=2001,2014 do begin
    
    for m=1,353,16 do begin
      
      name1='G:/CN/Minj/NDVI/byte/NDVI_MJ_QA_'+string(y,format='(i4)')+string(m,format='(I3.3)')
      printf,lun1, name1
      
    endfor
  
endfor

Free_lun,lun1

end