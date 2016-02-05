 PRO TIFF1
     
    ;定义文件路径
    MyRootDir='I:\AU\02\01\2\00' 

     help,data0
    ;遍历文件夹
    filearr = file_search(MyRootDir,'*.tif',count=num);
    
    fileindex=0
    
    name1=['RUNOFF-2001','RUNOFF-2002','RUNOFF-2003','RUNOFF-2004','RUNOFF-2005','RUNOFF-2006','RUNOFF-2007','RUNOFF-2008','RUNOFF-2009']
    data0 = make_array(4110,3474,12)
    name0 = make_array(12,/string)
    
    b=0
    c=108
    d=c+11
    FOR fileindex=c,d,1 DO BEGIN
       
    data0(*,*,b) = read_tiff(filearr[fileindex])
    name0(b)=filearr[fileindex]
    b=b+1
    ; inquire about this file; returns structure
    print,filearr[fileindex]
    endfor


      mc = [.5D,.5D, 112.905,-9.005]

      ps = [1D/100, 1D/100]

      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')

      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
    
    
    OUT_NAME1= 'F:\AU\1\'+name1[0]+'.img'
    print,OUT_NAME1
     ENVI_ENTER_DATA, data0,BNAMES=name0,map_info=map_info
    
;    for i=0, 4501-1 do begin
;         for j=0, 3501-1 do begin
;         
;         data0[i,j,fileindex]=nid[i,j]
;         name0[fileindex]=filearr[fileindex]
;        endfor
;      endfor
;        
        

    
;      mc = [.5D,.5D, 110.0,-10.]
;
;      ps = [1D/100, 1D/100]
;
;      units = ENVI_TRANSLATE_PROJECTION_UNITS('Degrees')
;
;      map_info = ENVI_MAP_INFO_CREATE(/geographic, mc=mc, ps=ps, units=units)
;    
;    
;    
;     ENVI_ENTER_DATA, data0,BNAMES=name0, map_info=map_info
;
;    delvar,data0,nid

    END