pro readNC

  finame = 'C:\1\1.nc'

  NCid = NCDF_OPEN(finame)
  NCinfo = NCDF_INQUIRE(NCid)
  print, NCinfo.Nvars 
  
; 查询NC文件中的变量，以及变量的 att 信息
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

; 读取变量名为 rhum 的数据
  Dataid = NCDF_VARID(NCid, 'rhum')
  NCDF_VARGET, NCid, Dataid, Data
  help, Data 

end