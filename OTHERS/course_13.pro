    ;Author:Sailor
    ;2013-12-25
    PRO Course_13
     
    ;定义文件路径
    MyRootDir='I:\RUNOFF\01'
     
    ;遍历文件夹
    filearr = file_search(MyRootDir,'*.nc',count=num);
    data0 = make_array(4110,3474,num)
    name0 = make_array(num,/string)
    help,name0
    VAR_name = "runoff_amount"
    
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

       for i=0, 4110-1 do begin
         for j=0, 3474-1 do begin
         
         data0[i,j,fileindex]=sst[i,j]
         name0[fileindex]=filearr[fileindex]
        endfor
      endfor

     help,data0 
        
    ENDFOR
    
    
;      mc = [.5D,.5D, 112.905,-8.985]
;
;      ps = [1D/100, 1D/100]
;
;      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
;
;      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
;      
;           
;    ENVI_ENTER_DATA, sst,BNAMES=filearr[fileindex], map_info=map_info
;         
         ENVI_ENTER_DATA, data0,BNAMES=name0, map_info=map_info

    
    END