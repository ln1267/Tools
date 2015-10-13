;Author:Sailor
;2013-12-25
PRO NC_read_single

  ;定义文件路径
  MyRootDir='E:\Data\Carbon_data\BGI\1'

  ;遍历文件夹
  filearr = file_search(MyRootDir,'*.nc',count=num);
  ;    data0 = make_array(4501,3501,num)

   VAR_name = "EnsembleLEcor_May12"

  
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


;    out_name = 'd:\au\spei\'+name0[fileindex]+'.tif'
;    write_tiff,out_name,sst,/float


    ;print,out_name


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
  ENVI_ENTER_DATA, sst, map_info=map_info
  ;;
  ;         ENVI_ENTER_DATA, data0,BNAMES=name0, map_info=map_info


END