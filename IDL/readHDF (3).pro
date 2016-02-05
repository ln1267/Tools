PRO readHDF

input_bat='E:/new/out/MOD16_hdf2tif.bat'

OpenW , lun2,input_bat,/Get_Lun

for n=2000, 2014 do begin

    for m=1, 12 do begin
    
    month=strmid(string(m+100),6)
    year=strmid(string(n),4)
    
    print,month,year
    input_name='E:/new/out/16/MOD16_'+year+'M'+month+'.prm'
    
    print, input_name
    
    OpenW , lun1,input_name,/Get_Lun
          
    Printf,lun1,'INPUT_FILENAME = E:\new\in\16\MOD16_'+year+'M'+month+'.hdf'
    Printf,lun1,''
    Printf,lun1,'SPECTRAL_SUBSET = ( 1 0 1 0 )'
    Printf,lun1,''
    Printf,lun1,'SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG'
    Printf,lun1,''
    Printf,lun1,'SPATIAL_SUBSET_UL_CORNER = ( 39.999999996 104.432583132 )'
    Printf,lun1,'SPATIAL_SUBSET_LR_CORNER = ( 29.999999997 103.923048442 )'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_FILENAME = E:\new\out/16/MOD16_'+year+'M'+month+'.tif'
    Printf,lun1,''
    Printf,lun1,'RESAMPLING_TYPE = NEAREST_NEIGHBOR'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_PROJECTION_TYPE = AEA'
    Printf,lun1,''
    Printf,lun1,'OUTPUT_PROJECTION_PARAMETERS = ( '
    Printf,lun1,' 0.0 0.0 25.0'
    Printf,lun1,' 47.0 105.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0'
    Printf,lun1,' 0.0 0.0 0.0 )'
    Printf,lun1,''
    Printf,lun1,'DATUM = WGS84'
       
    free_lun,lun1
    
    Printf,lun2,'resample -p E:/new/out/16/MOD16_'+year+'M'+month+'.prm'
  
   
  
    endfor
endfor

free_lun,lun2

END