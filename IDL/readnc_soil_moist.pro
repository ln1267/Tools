pro readNC

;  finame = 'F:\��ݺ�ģ��\NCEPNCAR Reanalysis 1 Surface\air.sig995.1948.nc'
finame = 'C:\1\1.nc'

  NCid = NCDF_OPEN(finame)
  NCinfo = NCDF_INQUIRE(NCid)
;  print, NCinfo.Nvars 
  
; ��ѯNC�ļ��еı������Լ������� att ��Ϣ
  FOR iVar = 0, NCinfo.Nvars-1 DO BEGIN
    Varinfo = NCDF_VARINQ(NCid, iVar)
    print, "Var Name: ", Varinfo.Name
    print, "Att Number: ", Varinfo.Natts 

    FOR iAtt = 0, Varinfo.Natts-1 DO BEGIN
      AttName = NCDF_ATTNAME(NCid, iVar, iAtt)
      NCDF_ATTGET, NCid, iVar, AttName, Att
      print, AttName, ":  ", STRING(Att)
    ENDFOR
    print, ""
  ENDFOR

; ��ȡ������Ϊ rhum �����
  Dataid = NCDF_VARID(NCid, Varinfo.Name)
  NCDF_VARGET, NCid, Dataid, Data
  help, Data 
  ENVI_ENTER_DATA, data,map_info=map_info
end