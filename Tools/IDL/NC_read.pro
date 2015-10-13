    ;Author:Sailor
    ;2013-12-25
    PRO NC_read
     
    ;定义文件路径
    MyRootDir='H:\AU\ECV_SM\ESACCI-L3S_SOILMOISTURE-SSMV-MERGED\1980'
     
    ;遍历文件夹
    filearr = file_search(MyRootDir,'*.nc',count=num);
;    data0 = make_array(4501,3501,num)

    help,name0
    VAR_name = "sm"
    
    name0=make_array(366,/string)
    a=0

    for year=1980,1980 do begin
    ; Whether it is "runnian"
      IF ((YEAR MOD 4) EQ 0 AND (YEAR MOD 100) NE 0) OR ((YEAR MOD 400) EQ 0) THEN BEGIN
      
      N_days=366
      ENDIF ELSE BEGIN
      N_days=365
      
      ENDELSE
    
    
    for m=1,N_days do begin
    name0[a]='ESA_SM_'+string(year,format='(i4)')+string(m,format='(i3.3)')
    print,name0[a]
    a=a+1
    Endfor
    Endfor
    
    FOR fileindex=0,num-1,1 DO BEGIN
    nid = ncdf_open(filearr[fileindex], /nowrite )
    ; inquire about this file; returns structure
    print,filearr[fileindex]
    file_info = ncdf_inquire( nid )
    ; print out the dimensions of this file
    FOR dimid=0, file_info.ndims -1 DO BEGIN
    ncdf_diminq, nid, dimid, name, size
    print, ' ---> dimension ' + name + ' is: ', size
    ENDFOR
    FOR varid=0, file_info.nvars-1 DO BEGIN
    ; inquire about the variable; returns structure
    var = ncdf_varinq( nid, varid )
    print,var
    print,'========================'
    ;read all attributes
    FOR var_att_id=0,var.natts -1 DO BEGIN
    att_name = ncdf_attname( nid, varid, var_att_id )
    print,att_name
    ncdf_attget, nid, varid, att_name, tematt
    print,string(tematt)
    ENDFOR
    ENDFOR
    ;read sst
    
    sstid = ncdf_varid(nid,VAR_name)
    
    ncdf_varget, nid, sstid, sst
      help, sst 
 
 ; Out nc documents to tif     
            
      out_name = 'H:\AU\ECV_SM\tif\'+name0[fileindex]+'.tif'
      write_tiff,out_name,sst,/float
    

print,out_name

        
    ENDFOR
    
    
;      mc = [.5D,.5D, 110.0,-10.0]
;
;      ps = [1D/100, 1D/100]
;
;      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
;
;      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
;;      
;;           
    ENVI_ENTER_DATA, sst,BNAMES=name0, map_info=map_info
;;         
;         ENVI_ENTER_DATA, data0,BNAMES=name0, map_info=map_info

    
    END