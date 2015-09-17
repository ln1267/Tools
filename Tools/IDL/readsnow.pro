Pro READSNOW



for y=2001,2001 do begin
  
  for day=1,1 do begin
 
    name='G:/CN/SNOW/25km/SI2001/'+string(y,format='(i4)')+string(day,format='(I3.3)')+'f.txt'
    openr,lun,name,/get_lun
    readf,lun 
    readf,lun
    readf,lun
    readf,lun
    readf,lun
    readf,lun
    readf,lun
    readf,lun,data,format='(f)'
    help,data
  endfor
  
  
  
  
endfor




END