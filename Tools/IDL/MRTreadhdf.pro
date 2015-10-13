PRO mrtreadHDF
; This pro is use for providing paprameters for MRT to read HDF 
input_bat='D:/Temp/LAI_GLASS/CN/LAI_hdf2tif.bat'

OpenW , lun2,input_bat,/Get_Lun

for n=2001, 2012 do begin
     
 ;    for m=1,12 do begin
    for m=1,361,8 do begin
 
 ;    month=strmid(string(m+100),6)
    month=strmid(string(m+1000),5)
    year=strmid(string(n),4)
    
    print,month,year
    input_name='D:/Temp/LAI_GLASS/CN/tif/LAI_CN'+year+month+'.prm'
    
    print, input_name
    
    OpenW , lun1,input_name,/Get_Lun
          
   ; Printf,lun1,'INPUT_FILENAME = D:/Temp/LAI_GLASS/CN/hdf/GLASS01A01.V03.A'+year+month+'.hdf'
    Printf,lun1,'INPUT_FILENAME = D:/Temp/LAI_GLASS\TmpMosaic.hdf'  
    Printf,lun1,''
    Printf,lun1,'SPECTRAL_SUBSET = ( 1 )'
    Printf,lun1,''
    Printf,lun1,'SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG'
    Printf,lun1,''
    Printf,lun1,'SPATIAL_SUBSET_UL_CORNER = ( 59.999999995 99.999999975  )'
    Printf,lun1,'SPATIAL_SUBSET_LR_CORNER = ( 9.999999999 121.851193415  )'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_FILENAME = D:/Temp/LAI_GLASS/CN/tif/LAI_CN'+year+month+'.tif'
    Printf,lun1,''
    Printf,lun1,'RESAMPLING_TYPE = NEAREST_NEIGHBOR'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_PROJECTION_TYPE = GEO'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_PROJECTION_PARAMETERS = ( '
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0 )'
    Printf,lun1,''
    Printf,lun1,'DATUM = WGS84'
       
    free_lun,lun1
    
    ; .bat 
    Printf,lun2,'mrtmosaic -s "1 0" -i D:/Temp/LAI_GLASS/CN/tif\LAI_CN'+year+month+'_mosaic.prm'+' -o D:/Temp/LAI_GLASS\TmpMosaic.hdf'
    Printf,lun2,'resample -p D:/Temp/LAI_GLASS/CN/tif/LAI_CN'+year+month+'.prm'
    Printf,lun2,'del D:/Temp/LAI_GLASS\TmpMosaic.hdf'
   
   
   
   ; mosaic
   
    mosaic_name='D:/Temp/LAI_GLASS/CN/tif/LAI_CN'+year+month+'_mosaic.prm'
    
    print, mosaic_name
    
    OpenW , lun3,mosaic_name,/Get_Lun
   
   
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h23v04.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h23v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h24v04.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h24v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h25v03.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h25v04.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h25v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h25v06.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h26v03.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h26v04.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h26v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h26v06.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h27v04.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h27v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h27v06.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h28v05.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h28v06.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h28v07.hdf'
    Printf,lun3,'D:\temp\LAI_GLASS\CN\hdf\GLASS01A01.V03.A'+year+month+'.h29v06.hdf' 
   
    Free_lun,lun3
       
    endfor
endfor

free_lun,lun2

END