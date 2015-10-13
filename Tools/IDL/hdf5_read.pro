; :Description:
;    read dataset from GOSAT document,FileType:*.H5
;  Author: wangchao
;  Email:wangchaoecnu@gmail.com
;  Date:2011-2-21

PRO hdf5_READ

 ; file_name = DIALOG_PICKFILE(/read,filter = '*.hdf5')

  For year=2014,1997,-1 do begin
  
    for month=12,1,-1 do begin
    
    file_name='M:\AU\Fire\GFED4\GFED4.1s_'+string(year,format='(I4)')+'.hdf5'
    group_name = '/burned_area/'+string(month,format='(I2.2)')
    Dataset_name = 'burned_fraction'
    Bname='GFED4_Burned_frac'+string(year,format='(I4)')+string(month,format='(I2.2)')
   print, file_name
   print, group_name
   
    IsHdf = H5F_IS_HDF5(file_name)
    IF IsHDF EQ 0 THEN RETURN
    
    file_id = H5F_OPEN(file_name)
    
    group_id   = H5G_OPEN(file_id,group_name)
    
    dataset_id = H5D_OPEN(group_id,dataset_name)
    data = H5D_READ(dataset_id)
  
    
    H5D_CLOSE,dataset_id
    H5G_CLOSE,group_id
    H5F_CLOSE,file_id
    
          mc = [.5D,.5D,-179.875,89.875]
;
      ps = [1D/4, 1D/4]

      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
;
      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
;;     
    
    
    ENVI_ENTER_DATA,data,Bnames=Bname,map_info=map_info
    
    endfor            
  endfor
  
END