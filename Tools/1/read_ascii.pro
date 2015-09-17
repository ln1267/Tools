pro read_ascii
file_name=dialog_pickfile()
print,file_name
openr,lun1,file_name,/get_lun
for i=1,6 do begin
line='1'
readf,lun1,line
print,line
endfor
tempstr=make_array(7321,4357)
readf,lun1,tempstr,format='(7321f6.1)'
openw,lun2,"output.txt",/get_lun
printf,lun2," ncols      7321"
printf,lun2," nrows      4357"
printf,lun2," xllcorner  73.497955"
printf,lun2," yllcorner  17.067215"
printf,lun2," cellsize   0.008421"
printf,lun2," NODATA_value  -99.00"
printf,lun2,tempstr,format='(7321f7.1)'
 free_lun,lun2



end