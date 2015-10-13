Pro readtxt



  name=make_array(156,/string)
  name1=make_array(156,/string)
  a=0

  for y=1990,2012,1 do begin
  
    for m=1,12,1 do begin
    
    name[a]='TEM_50km_'+string(y,format='(i4)')+string(m,format='(I2.2)')
    name1[a]='C:\Users\ning\Downloads\TEM_50km\SURF_CLI_CHN_TEM_MON_GRID_0.5-MAX-'+string(y,format='(i4)')+string(m,format='(I2.2)')
    a=a+1
    endfor
  endfor
  
  print,dims0






END
