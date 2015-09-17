; read_modis_hdf.pro
pro readhdf,lon,lat,data
;
for year=2003,2003 do begin

        ; Whether it is "runnian"
          IF ((YEAR MOD 4) EQ 0 AND (YEAR MOD 100) NE 0) OR ((YEAR MOD 400) EQ 0) THEN BEGIN
          
          N_days=366
          ENDIF ELSE BEGIN
          N_days=365
          
          ENDELSE
    Cu_day=1
    For month=4,12 do begin
   
       If N_days eq 365 then begin
          
            case month of
               1: N_M_days=31
               2: N_M_days=28
               3: N_M_days=31
               4: N_M_days=30
               5: N_M_days=31
               6: N_M_days=30
               7: N_M_days=31
               8: N_M_days=31
               9: N_M_days=30
               10: N_M_days=31
               11: N_M_days=30
               12: N_M_days=31
            ENDCASE
       
          ENDIF ELSE BEGIN
               
           Case month of
               1: N_M_days=31
               2: N_M_days=29
               3: N_M_days=31
               4: N_M_days=30
               5: N_M_days=31
               6: N_M_days=30
               7: N_M_days=31
               8: N_M_days=31
               9: N_M_days=30
               10: N_M_days=31
               11: N_M_days=30
               12: N_M_days=31
            ENDCASE
          ENDELSE
          
        ;  data = make_array(1440,720,N_M_days,type = float)
      name = make_array(3,/string)
      
      name[0]='L:\linshi\zip\SV05_VG2_S10_VI_'+string(year,format='(i4)')+string(month,format='(i2.2)')+'10\0001\0001_NDV.hdf'
      name[1]='L:\linshi\zip\SV05_VG2_S10_VI_'+string(year,format='(i4)')+string(month,format='(i2.2)')+'20\0001\0001_NDV.hdf'
      name[2]='L:\linshi\zip\SV05_VG2_S10_VI_'+string(year,format='(i4)')+string(month,format='(i2.2)')+string(N_M_days,format='(i2.2)')+'\0001\0001_NDV.hdf'
      
          For num=0,2 do begin
          
            ; Open an HDF file and and get file ID
            flname=name[num]
            file_id=HDF_OPEN(flname,/READ)

            ; Get SD interface ID
            sd_interface_id=HDF_SD_START(flname,/READ)

            ; Get info about the file
            ;  This returns the number of datasets and user variable attributes
            HDF_SD_FILEINFO,sd_interface_id,n_datasets,attributes
            print,'n_datasets=',n_datasets
            print,'sd_interface_id=',sd_interface_id

            ; Get info of a given dataset
            for i=0,n_datasets-1 do begin
              print,i,sd_interface_id
              si_sds_id=HDF_SD_SELECT(sd_interface_id,i)
              HDF_SD_GETINFO,si_sds_id,HDF_TYPE=si_hdf_type,$
                DIMS=si_dims, NDIMS=si_ndims,NAME=si_name,NATTS=si_natts
              print,i,si_name,si_hdf_type,si_dims,$
                format='(i0,". ",a,t53,a,t65," size: ",5(i0,:,"x"))'

            endfor
            if n_datasets eq 0 then stop,'no data'


            ; Get data and store in array data
            HDF_SD_GETDATA,si_sds_id,data
            ; print,s1_name,' Minimum=',Min(data),'  Maximum=',max(data)
            
            out_name = 'L:\linshi\SPOT\NDVI_SPOT_'+string(year,format='(i4)')+string(month,format='(i2.2)')+string(num,format='(i2.2)')+'.tif'
            band_name='NDVI_SPOT_'+string(year,format='(i4)')+string(month,format='(i2.2)')+string(num,format='(i2.2)')
            write_tiff,out_name,data
            
            
          ;  ENVI_ENTER_DATA, data,BNAMES=band_name, map_info=map_info
            ; Close the interface
            HDF_SD_ENDACCESS,si_sds_id


          endfor
  endfor  
endfor    
;
;L:\linshi\zip\SV04_VG1_S10_VI_19990101\0001
;
;
;




;; Select the datasets considered
;s1_sds_indx=0
;;s1_sds_indx=9 ;for AOD
;print,''
;;read,'Enter the index number of datasets ',s1_sds_indx
;s1_sds_id = HDF_SD_SELECT(sd_interface_id,s1_sds_indx)
;HDF_SD_GETINFO,s1_sds_id,HDF_TYPE=s1_hdf_type,$
; DIMS=s1_dims, NDIMS=s1_ndims,NAME=s1_name,NATTS=s1_natts
; print,i,si_name,si_hdf_type,si_dims,$
;   format='(i0,". ",a,t53,a,t65," size: ",5(i0,:,"x"))'
;; Loop over attributes for the given SDS
;for n=1,s1_natts-1 do begin
; HDF_SD_ATTRINFO,s1_sds_id,n,NAME=ss_name,DATA=ss_attr_dat,$
;  COUNT=ss_count,HDF_TYPE=ss_hdftype,TYPE=ss_type
; 
;endfor



end
