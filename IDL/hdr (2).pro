Pro hdr 
a=1
for y=2001,2014 do begin
  for m=1,23 do begin
  
  name='J:/NDVI/Timesat_result/Byte/NDVI_MJ1/NDVI_MJ1_'+string(a,format='(i4.4)')+'.HDR'
  print,name
  openw,lun,name,/get_lun
  printf,lun,'ENVI'
  printf,lun,'samples = 638'
  printf,lun,'lines   = 1015'
  printf,lun,'bands   = 1'
  printf,lun,'header offset = 0'
  printf,lun,'file type = ENVI Standard'
  printf,lun,'data type = 1'
  printf,lun,'interleave = bsq'
  printf,lun,'sensor type = Unknown'
  printf,lun,'byte order = 0'
  printf,lun,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, 102.45506314, 33.16366245, 2.5432800000e-003, 2.5432800000e-003, WGS-84, units=Degrees}'
  printf,lun,'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  printf,lun,'wavelength units = Unknown'
  
  Free_lun,lun
  a=a+1
  endfor
endfor

end
