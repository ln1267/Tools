;trend analysis
PRO trend


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  
  ; For WUE cacluation
;  map_info = envi_get_map_info(fid=fid0)
;  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
;  
;   ENVI_SELECT, fid=fid1,dims=dims1,pos=po1
;    map_info = envi_get_map_info(fid=fid1)
;  ENVI_FILE_QUERY, fid1, dims=dims1, nb=nb1,data_type = type1
;  data02 = make_array(dims1[2]+1,dims1[4]+1,nb1,type = float)
;  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
;  data0= make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
;  data1 = make_array(dims0[2]+1,dims0[4]+1,type = float)
;    for i=0, nb0-1 do begin
;    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
;    data02[*,*,i] = envi_get_data(fid = fid1,dims = dims1, pos = i)
;    data0[*,*,i]=data01[*,*,i]/data02[*,*,i]
;  endfor
  ;
  ;
  ;
  ;
  ;data file
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  data0 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = float)
  data1 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor

  ;processing
 
; //caculating by each pixel  
  b1=make_array(nb0,type = type0)
  b2=0
  b3=0
  b4=0
  sl=0

for a=0, dims0[2] do begin
for b=0, dims0[4] do begin
for i=0, nb0-1 do begin

b1(i)=float(data0[a,b,i])

endfor

sl=mkstrend(b1)

data1[a,b]=sl
;print,sl
endfor
endfor
ENVI_ENTER_DATA, data1,map_info=map_info

; //caculating by spatial  - Ning
;  b1=0
;  b2=0
;  b3=0
;  b4=0
;  sl=0
;
;for i=0, nb0-1 do begin
;
;b1=float(data0[*,*,i])
;
;endfor
;
;sl=mkstrend(b1)

;data1[a,b]=sl
;print,sl
;ENVI_ENTER_DATA, data1,map_info=map_info



; //Original caculating by spatial
;  b1= 0
;  b2= 0
;  b3= 0
;  b4= 0
;   
;  for i=0, nb0-1 do begin
;    b1= float(data0[*,*,i])*(i+1)+b1
;    b2= float(data0[*,*,i])+b2
;    b3= float(i+1+b3)
;    b4= float((i+1)^2+b4)
;   
;  endfor
;
;b5=(b1-(b2*b3/nb0))/(b4-(b3^2/nb0))
;
;ENVI_ENTER_DATA, b5,map_info=map_info

end



function mkstrend,x,undef=undef

;It uses the non parametric mann-kendal (mk) statistics to determine
;if there is (or not) a signifcant trend 95%. The slope is calcu-
;lated by using the Sen's method(s). S is robust and less affected by
;outliers (sen p.k. 1968, ASAJ).
;ex. sl=mkstrend(x,undef=-999.).
; where x is your ts: x=[0.5, 0.1, 0.2, 0.1, 0.3]
; no slope then sl=undef. if the ts has one nan: sl=undef

undef=-999.

if total(finite(x,/nan)) eq 0 then begin

x=float(x)
nx=n_elements(x)
nx1=nx-1.
n=nx*(nx-1)/2. ; The number of elements in d
d=fltarr(n)
m=0.

for i=0,nx1-1 do begin

for j=i+1,nx-1 do begin

d(m)=x(j)-x(i)
m=m+1

endfor

endfor

for i=0L,n-1 do begin

if d(i) lt 0 then d(i)=-1.
if d(i) eq 0 then d(i)= 0.
if d(i) gt 0 then d(i)= 1.

endfor

s=total(d)

U=x(uniq(x(sort(x))))
Corr=0 ;Correction for tied observations (equal value)

for y=0,n_elements(U)-1 do begin

find=where(x eq U(y))
uj=n_elements(find)
Corr=Corr+uj*(uj-1)*(2*uj+5)

endfor


Vs=(nx*(nx-1.)*(2*nx+5.)-Corr)/18. ;For long series it is necessary to use the whole
;eq. 2.6 (Corr) (Sen p.k. 1968,ASAJ)

if s gt 0. then z=(s-1)/sqrt(Vs)
if s lt 0. then z=(s+1)/sqrt(Vs)
if s eq 0. then z=0.

nor=gauss_cvf(0.025) ; Prob at 95% (two-side)

;The slope

Sn=fltarr(n)
m=0.

for i=0,nx1-1 do begin

for j=i+1,nx-1 do begin

Sn(m)=(x(i)-x(j))/(i-j)
m=m+1

endfor

endfor


Snsorted=Sn(sort(Sn))
m=float(fix(n/2.))

if abs(z) lt nor then begin

slope=undef

endif else begin

if 2*m eq n then slope=0.5*(Snsorted(m)+Snsorted(m+1))
if 2*m+1. eq n then slope=Snsorted(m+1)

endelse

endif else begin

slope=undef

endelse

return, slope

end

;-----------------------------------------------------
;trend v value
PRO trendv


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
  ;
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  data0 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = type0)
  for i=0, nb0-1 do begin
    data0[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  ;
  ;processing
  
  b1= 0
  b2= 0
  b3= 0
  b4= 0
   
  for i=0, nb0-1 do begin
    b1= data0[*,*,i]+b1
   endfor
  ;
   b1=b1/nb0
  ;
  for i=0, nb0-2 do begin
    b2= float((data0[*,*,i]-data0[*,*,i+1]))^2+b2
  endfor
  ;
  for i=0, nb0-1 do begin
    b3= float((data0[*,*,i]-b1))^2+b3
  endfor
  ;
   b4=b2/b3

ENVI_ENTER_DATA, b4,map_info=map_info

end


PRO trendp


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
  ;
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = type0)
  data0= make_array(dims0[2]+1,dims0[4]+1,type = float)
  data1 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  data2 = make_array(dims0[2]+1,dims0[4]+1,type = float)
  for i=0, nb0-1 do begin
    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  ;
  ;processing
  
 b1=make_array(nb0,type = type0)
 b2=make_array(nb0,type = type0)
  b3= 0
  b4= 0

for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  b1(i)=float(data01[a,b,i])
  
  b2(i)=float(i+1)     
  endfor
  
  data0(a,b)=regress(b2,b1, SIGMA=sigma, CONST=const, $
     MEASURE_ERRORS=measure_errors)
  data1(a,b)=const
  data2(a,b)=CORRELATE(b2,b1)
  endfor
  endfor
  help,data0,data1,data2
  ENVI_ENTER_DATA, data0,Bnames='regress',map_info=map_info
  
  ENVI_ENTER_DATA, data1,Bnames='constant',map_info=map_info
  ENVI_ENTER_DATA, data2,Bnames='Corelate', map_info=map_info
  
  
END


PRO std


  ENVI_SELECT, fid=fid0,dims=dims0,pos=pos
  ;
  ;
  map_info = envi_get_map_info(fid=fid0)
  ENVI_FILE_QUERY, fid0, dims=dims0, nb=nb0,data_type = type0
  
  data01 = make_array(dims0[2]+1,dims0[4]+1,nb0,type = type0)
  data0= make_array(dims0[2]+1,dims0[4]+1,type = float)
  for i=0, nb0-1 do begin
    data01[*,*,i] = envi_get_data(fid = fid0,dims = dims0, pos = i)
  endfor
  ;
  ;processing
  
 b1=make_array(nb0,type = type0)
 b2=make_array(nb0,type = type0)
  b3= 0
  b4= 0

for a=0, dims0[2] do begin
  for b=0, dims0[4] do begin
   
  for i=0, nb0-1 do begin
  
  b1(i)=float(data01[a,b,i])

  endfor
  
  data0(a,b)=stddev(b1)

  endfor
  endfor
  help,data0
  ENVI_ENTER_DATA, data0,Bnames='STD',map_info=map_info

  
  
END