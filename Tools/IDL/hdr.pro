Pro hdr

for y=2011,2013 do begin
  for m=1,12 do begin
  ;for num=0,2 do begin
 ; name='L:/linshi/tif/month/ESA_SM_'+string(y,format='(i4.4)')+string(m,format='(i2.2)')+string(num,format='(i2.2)')+'.HDR'
   name='L:/SM_NDVI_spot\ESA_SM_tif/month/ESA_SM_'+string(y,format='(i4.4)')+string(m,format='(i2.2)')+'.HDR'
  print,name
  openw,lun,name,/get_lun
  printf,lun,'ENVI'
  printf,lun,'samples = 1440'
  printf,lun,'lines   = 720'
  printf,lun,'bands   = 1'
  printf,lun,'header offset = 0'
  printf,lun,'file type = TIFF'
  printf,lun,'data type = 4'
  printf,lun,'interleave = bsq'
  printf,lun,'sensor type = Unknown'
  printf,lun,'byte order = 0'
  printf,lun,'read procedures = {idl_tiff_read_spatial, idl_tiff_read_spectral}'
  printf,lun,'map info = {Geographic Lat/Lon, 1.5000, 1.5000, -179.87500000, 89.87500000, 2.5000000000e-001, 2.5000000000e-001,  WGS-84, units=Degrees}'
  printf,lun,'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  printf,lun,'wavelength units = Unknown'
  
  Free_lun,lun
  
  ;endfor
  endfor
endfor

end
