; input:
; FPAR/LAI MOD15A2   Fpar_1km 0.01; Lai_1km 0.1  see: https://lpdaac.usgs.gov/products/modis_products_table/mcd15a2
; ALBEDO MOD43C3  Albedo_WSA_Band_shortwave (White Sky Albedo) 0.0010   see: https://lpdaac.usgs.gov/products/modis_products_table/mcd43c3
; Tair， Tmax, Tmin, Tannual
; Radiation derived from SSD
; 
; PF 
;1NF代表针叶林
;2BF代表阔叶林
;3MF代表混交林（针阔混交）
;4BBF代表竹林
;5SHB代表特灌林
;6UNKNOWN未知（总共2处）

function ET, et_out, FLAGdaynight, slat, day, Tin, Tmin, Fc, ndvi, Tday, Tnight, RH, elev, Pa, albedo, LAI, $
           Tmin_open, Tmin_close, VPD_close, VPD_open, gl_sh, gl_e_wv, Cl, RBL_MIN, RBL_MAX, Rs
  ;-------------------------------------------------------------------
  ; set constant
  ;               ENF  EBF  DNF   DBF  MF  CSH   OSH   WL    SV   GRASS CROP
  ;                0   1    2     3   4    5     6     7     8    9     10
  M_Tmin_open = [8.31,9.09,10.44,9.94,9.50,8.61,8.80,11.39,11.39,12.02,12.02]
  M_Tmin_close= [-8.0,-8.0,-8.0,-6.00,-7.0,-8.0,-8.0,-8.00,-8.00,-8.00,-8.00]
  M_VPD_close = [3000,4000,3500, 2900,2900,4300,4400, 3500, 3600, 4200, 4500]
  M_VPD_open  = [ 650,1000, 650,  650, 650, 650, 650,  650,  650,  650,  650]
  M_gl_sh     = [0.01,0.01,0.01, 0.01,0.01,0.02,0.02, 0.04, 0.04, 0.02, 0.02]
  M_gl_e_wv   = [0.01,0.01,0.01, 0.01,0.01,0.02,0.02, 0.04, 0.04, 0.02, 0.02]
  M_Cl        = [0.24,0.24,0.24, 0.24,0.24,0.55,0.55, 0.55, 0.55, 0.55, 0.55]*0.01
  M_RBL_MIN   = [60.0,60.0,60.0, 60.0,60.0,60.0,60.0, 60.0, 60.0, 60.0, 60.0]
  M_RBL_MAX   = [95.0,95.0,95.0, 95.0,95.0,95.0,95.0, 95.0, 95.0, 95.0, 95.0]

  LRstd     = 0.0065       ;(-K/m) standard temperature lapse rate
  Tstd      = 288.15       ;(K) standard temp at 0.0 m elevation
  Gstd      = 9.80665      ;(m/s2) standard gravitational accel.
  RR        = 8.3143       ;(m3 Pa/ mol K) gas law constant
  MA        = 28.9644e-3   ;(kg/mol) molecular weight of air
  MW        = 18.0148e-3   ;(kg/mol) molecular weight of water */
  Pstd      = 101325.0     ;(Pa) standard pressure at 0.0 m elevation
  g_cu      = 0.00001      ;(m/s) cuticular conductance per unit LAI, see p1785 of Mu et al.(2011)
  epsiron   = 0.6219       ;(MW/MA) unitless ratio of molec weights */

  StevenBoze= 5.673e-8     ;(W/(m2 K4)) Stefan-Boltzmann constant
  Cp        = 1012.0       ;(J/kg K) specific heat of air

  ;-------------------------------------------------------------------
  weiduhudu = slat*3.1415926/180.0
  cipianjiao= 0.4093*sin(2.0*3.1415926*day/365.0-1.405)
  chiweihudu= cipianjiao
  a         = sin((90.27*3.1415926/180.0+weiduhudu-chiweihudu)/2.0);
  b         = sin((90.27*3.1415926/180.0-weiduhudu+chiweihudu)/2.0);
  daylen    = 4.0/15.0*asin(sqrt((a*b)/(cos(weiduhudu)*cos(chiweihudu))))*180.0/3.1415926
  daylen    = (daylen gt 0.0)*(daylen lt 24.0)*daylen;
;  dr        = 1.0-0.01673*cos(2*3.1415926*day/365.0)
;  Ws        = acos(-tan(weiduhudu)*tan(chiweihudu))
;  ; Ra: rmmd-1
;; Ra        = 15.54*(Ws*sin(weiduhudu)*sin(chiweihudu)+cos(weiduhudu)*cos(chiweihudu)*sin(Ws))/dr^2
;  ; Ra: MJ m-2 day-1
;  Ra        = 37.6*dr*(Ws*sin(weiduhudu)*sin(chiweihudu)+cos(weiduhudu)*cos(chiweihudu)*sin(Ws))
;  Rs        = float(FLAGdaynight) * (1-0.23)*(0.21+0.56*SSD/daylen)*Ra
;  esat      = (Tin gt 0)*(6.11*10^(7.63*Tin/(241.9+Tin)))+(Tin le 0)*(6.11*10^(9.5*Tin/(265.5+Tin)))
;  e         = RH/100.0*esat
;  Rbl       = 1.9838e-9*(0.3+0.7*SSD/daylen)*(0.32-0.026*sqrt(e))*(Tin+273.15)^4
;  Rnet      = (1.0-albedo)*Rs/0.0864 - Rbl
;  Mu et al. method  
  epxiron_a = 1.0-0.26*exp(-7.77e-4*Tin^2)
  ; here convert to MJ m-2 day-1, as H.A. Cleugh et al.(2007) indicates Rs (MJ m-2 day-1)
  ; adjust Rs to daytime Rs, because cfsr Rs was calculated by the mean of 4-time obs
  Rs        = Rs*24.0/daylen
  Rnet      = (1.0-albedo)*(float(FLAGdaynight)*Rs/0.0864) + (epxiron_a-0.97)*stevenboze*(273.15+Tin)^4
  ; here convert to W m-2, other than that, the ET will be very large!
  A         = Rnet*0.0864 > 0;     
  Ac        = Fc*A
;  G_soil    = (Tmin_close le T_annual)*(T_annual lt 25.0)*(Tday-Tnight ge 5.0)*(4.73*Tday-20.87)
;  G         = G_soil*(1.0-Fc)
;  ;  G_soil(where(ABS(G) gt 0.39*ABS(A))) = 0.39*A
;  
;  G = 0.39*A*(1-Fc) *(ABS(G) gt 0.39*ABS(A)) + G*(ABS(G) le 0.39*ABS(A))
;  A_soil    = (1.0-Fc)*A-G
  G  =  (-0.27*((ndvi/10000.0 >0) < 1.0)+0.39)*A > 0
  A_soil    = (1.0-Fc)*A-G  >  0
  
  ;-------------------------------------------------------------------
  dt     = 0.2;     /* set the temperature offset for slope calculation */
  ;/* calculate temperature offsets for slope estimate */
  t11 = Tin+dt;
  t22 = Tin-dt;
  ;/* calculate saturation vapor pressures at t1 and t2 */
  pvs1 = 610.7 * exp(17.38 * t11 / (239.0 + t11));
  pvs2 = 610.7 * exp(17.38 * t22 / (239.0 + t22));
  ;/* calculate slope of pvs vs. T curve, at ta */
  s = (pvs1-pvs2) / (t11-t22);
  ;     s      = 4098.0*(610.8*exp(17.27*Tin/(Tin+237.3)))/(Tin+237.3)^2 ; Tin: oC

  rho    = 1.292 - (0.00428 * Tin) ; tair oC; density of air (rho) as a function of air temperature
  esat   = (Tin gt 0)*(6.11*10^(7.63*Tin/(241.9+Tin)))+(Tin le 0)*(6.11*10^(9.5*Tin/(265.5+Tin)))
  e      = RH/100.0*esat
  VPD    = esat-e
  Fwet   = (RH ge 70)*(RH*0.01)^4
;  Fwet   = (RH ge 70)*(RH*0.008)^4
  ;-------------------------------------------------------------------
  ;rs
  t1     = 1.0-LRstd*elev/(Tstd)
  t2     = Gstd/(LRstd*RR/MA)
  Pa     = Pstd*t1^t2
  rcorr  = 1.0/(101300.0/Pa*((Tin+273.15)/293.15)^1.75)
  mTmin  = (Tmin ge Tmin_open)*1.0+(Tmin le Tmin_close)*0.1
  mTmin  = mTmin + (Tmin lt Tmin_open)*(Tmin gt Tmin_close)*((Tmin-Tmin_close)/(Tmin_open-Tmin_close))
  mVPD   = (VPD le VPD_open)*1.0+(VPD ge VPD_close)*0.1
  mVPD   = mVPD  + (VPD gt VPD_open)*(VPD lt VPD_close)*((VPD_close-VPD)/(VPD_close-VPD_open))
;Gs
;  Gs_day1= Cl*mTmin*mVPD*rcorr
;  Gs_night1 = 0.0
  Gs_daynight= float(FLAGdaynight) * Cl*mTmin*mVPD*rcorr
  Gcu    = g_cu*rcorr
  Gs2    = gl_sh
  Cc_i   = (LAI gt 0)*((1.0-Fwet) gt 0)*1.0* (Gs2*(Gs_daynight+Gcu)/(Gs_daynight+Gs2+Gcu))*LAI*(1.0-Fwet)
  rs_i   = 1.0/Cc_i

  ;ra
  gl_bl  = gl_sh
  rhrh     = 1.0/gl_bl
  rr     = rho*Cp/(4.0*stevenboze*(Tin+273.15)^3)
  ra     = rhrh*rr/(rhrh+rr)
  ;-------------------------------------------------------------------
  ; canopy evaporation /* calculate evaporation, in W/m^2  */
  ;
  rhc = 1.0/(gl_sh*LAI*Fwet)
  rrc = rr
  rhrc= rhc*rrc/(rhc+rrc)
  rvc = 1.0/(gl_e_wv*LAI*Fwet)
  lhvap = 2.5023e6 - 2430.54 * Tin  ; (J/kg) latent heat of vaporization as a function of ta
  lamtaE_wetcanopy = (s*Ac*Fc+rho*Cp*(esat-e)*Fc/rhrc)*Fwet/(s+Pa*Cp*rvc/(lhvap*epsiron*rhrc))
;  lamtaE_wetcanopy = (s*Ac+rho*Cp*(esat-e)*Fc/rhrc)*Fwet/(s+Pa*Cp*rvc/(lhvap*epsiron*rhrc))   ; not double use of Fc here
  ; evap_canopy ; convert from mm/s to mm/day or mm/night
  if FLAGdaynight eq 1 then begin
    et_out[*,*,0]      = lamtaE_wetcanopy /(2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
  endif else begin
    et_out[*,*,0]      = lamtaE_wetcanopy /(2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen)  > 0
  endelse
  ;-------------------------------------------------------------------
  ; soil evaporation
  rtotc  = rbl_max*(VPD le VPD_open)+rbl_min*(VPD ge VPD_close)
  rtotc  = rtotc + (VPD gt VPD_open)*(VPD lt VPD_close)*(rbl_max-(rbl_max-rbl_min)*(VPD_close-VPD)/(VPD_close-VPD_open))
  r_tot  = rtotc*rcorr
  r_as   = r_tot*rr/(r_tot+rr)
  gama   = MA/MW * (Cp*Pa/lhvap)  ; (J/kg)        latent heat of vaporization of water
  lamtaE_wetsoil = (s*A_soil+rho*Cp*(1.0-Fc)*VPD/r_as)*Fwet/(s+gama*r_tot/r_as)
  lamtaE_soilpot = (s*A_soil+rho*Cp*(1.0-Fc)*VPD/r_as)*(1-Fwet)/(s+gama*r_tot/r_as)
  ;///////////////////////
  lamtaE_soil    = lamtaE_wetsoil + lamtaE_soilpot*(RH/100.0)^(VPD/200.0)
  
  ; evap_soil ; convert from mm/s to mm/day or mm/night
  if FLAGdaynight eq 1 then begin
    et_out[*,*,1]    = lamtaE_soil    / (2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
    et_out[*,*,4]    = lamtaE_wetsoil / (2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
    et_out[*,*,5]    = lamtaE_soilpot / (2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
  endif else begin
    et_out[*,*,1]    = lamtaE_soil   /  (2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen) > 0
    et_out[*,*,4]    = lamtaE_wetsoil / (2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen) > 0;24.0  > 0;
    et_out[*,*,5]    = lamtaE_soilpot / (2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen) > 0;24.0  > 0;
  endelse
  ;-------------------------------------------------------------------
  ; transpiration
  ;  old gama   = 1005.0*Pa/(622.0*(2500.0-2.4*Tin)) ; tair: oC; P Pa
  ;  bgc gama   = MA/MW * (Cp*Pa/lhvap)  ; (J/kg)        latent heat of vaporization of water
  ;
  ; lamtaE : (W/m2)        latent heat flux density
  lamtaE = (s*Ac*Fc + rho*Cp*(esat-e)*Fc/ra)*(1.0-Fwet)/(s+gama*(1.0+rs/ra))
;  lamtaE = (s*Ac + rho*Cp*(esat-e)*Fc/ra)*(1.0-Fwet)/(s+gama*(1.0+rs/ra)) ; not double use of Fc here
  lamtaPE= 1.26*s*Ac*(1.0-Fwet) /(s+gama);
  ; lamta: MJ/kg
  ; transpiration     (kg/m2/s)     water vapor mass flux density  
  ; convert from mm/s to mm/day or mm/night
  if FLAGdaynight eq 1 then begin
    et_out[*,*,2] = lamtaE / (2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
    et_out[*,*,6] = lamtaPE/ (2.5023e6 - 2430.54 *Tin)*3600.0*daylen > 0;24.0  > 0;
  endif else begin
    et_out[*,*,2] = lamtaE / (2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen)  > 0
    et_out[*,*,6] = lamtaPE/ (2.5023e6 - 2430.54 *Tin)*3600.0*(24.0-daylen)  > 0
  endelse
  
  et_out[*,*,3] = (et_out[*,*,0]>0)+(et_out[*,*,1]>0)+(et_out[*,*,2]>0)
  et_out[*,*,7] = (et_out[*,*,0]>0)+(et_out[*,*,4]>0)+(et_out[*,*,5]>0)+(et_out[*,*,6]>0)
  return, et_out; three bands: evap_canopy, evap_soil, transpiration 
  
end
  
  
PRO modiset
  envi, /restore_base_save_files
  envi_batch_init
;-------------------------------------------------------------------
; set constant
;  see http://ntsg.umt.edu/project/mod16 readme file in 2013
;               ENF  EBF  DNF   DBF  MF  CSH   OSH   WL    SV   GRASS CROP
;                0   1    2     3   4    5     6     7     8    9     10
M_Tmin_open = [8.31,9.09,10.44,9.94,9.50,8.61,8.80,11.39,11.39,12.02,12.02]
M_Tmin_close= [-8.0,-8.0,-8.0,-6.00,-7.0,-8.0,-8.0,-8.00,-8.00,-8.00,-8.00]
M_VPD_close = [2500,3900,3500, 2800,2700,3300,3700, 3300, 3600, 3900, 3800]
M_VPD_open  = [ 650, 930, 650,  650, 650, 650, 650,  650,  650,  650,  650]
M_gl_sh     = [0.01,0.01,0.01, 0.01,0.01,0.02,0.02, 0.04, 0.04, 0.02, 0.02]
M_gl_e_wv   = [0.01,0.01,0.01, 0.01,0.01,0.02,0.02, 0.04, 0.04, 0.02, 0.02]
M_Cl        = [0.24,0.24,0.24, 0.24,0.24,0.55,0.55, 0.55, 0.55, 0.55, 0.55]*0.01
M_RBL_MIN   = [60.0,60.0,60.0, 60.0,60.0,60.0,60.0, 60.0, 60.0, 60.0, 60.0]
M_RBL_MAX   = [95.0,95.0,95.0, 95.0,95.0,95.0,95.0, 95.0, 95.0, 95.0, 95.0]

  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\landcoverType1-IGBP-halfDegree', r_fid=fid, /no_interactive_query, /no_realize ; landcover, forest type
  
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\bu\LAI-1982-2011-halfdegree-fit8day' , r_fid=fid0, /no_interactive_query, /no_realize ; LAI MOD15A2
;  ENVI_OPEN_FILE, 'C:\Users\zyu\Desktop\wue\MOD15A2\out\newLAI&FPAR\interpolate-fpar', r_fid=fid1, /no_interactive_query, /no_realize ; FPAR
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\bu\Fpar-1982-2011-halfdegree-fit8day', r_fid=fid1, /no_interactive_query, /no_realize ; FPAR
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-albedo-1982-2012', r_fid=fid2, /no_interactive_query, /no_realize ; ALBEDO
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-tmp-1982-2013', r_fid=fid3, /no_interactive_query, /no_realize ; Tmean Tair
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-tmx-1982-2013', r_fid=fid4, /no_interactive_query, /no_realize ; Tmax
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-tmn-1982-2013', r_fid=fid5, /no_interactive_query, /no_realize ; Tmin
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-RH-1982-2013', r_fid=fid6, /no_interactive_query, /no_realize ; RH
;  ENVI_OPEN_FILE, 'C:\Users\zyu\Desktop\wue\interpolate\in-SSD', r_fid=fid7, /no_interactive_query, /no_realize ; SSD
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-issol-1982-2013', r_fid=fid10, /no_interactive_query, /no_realize ; ISSOL
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\8day-NDVI-1982-2012', r_fid=fid8, /no_interactive_query, /no_realize ; NDVI

  ; ANNUAL TEMPERATURE, EQUAL TO THE NUMBER OF YEARS
;  ENVI_OPEN_FILE, 'C:\Users\zyu\Desktop\wue\interpolate\in-Tann', r_fid=fid8, /no_interactive_query, /no_realize ; annual temperature
  ENVI_OPEN_FILE, 'E:\ModisET-GlobalInput\dem', r_fid=fid9, /no_interactive_query, /no_realize ; dem
  
  map_info = envi_get_map_info(fid=fid2)
  psizex = map_info.ps(0)
  psizey = map_info.ps(1)
  xstart = map_info.mc(2)
  ystart = map_info.mc(3)
  ENVI_FILE_QUERY, fid0, dims=dims1, nb=nb1,data_type = type1,BNAMES=name0

    landcover = envi_get_data(fid =  fid,dims = dims1, pos = 0)
    elev      = envi_get_data(fid = fid9,dims = dims1, pos = 0)
    
    Tmin_open = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    Tmin_close= make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    VPD_close = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    VPD_open  = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    gl_sh     = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    gl_e_wv   = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    Cl        = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    RBL_MIN   = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
    RBL_MAX   = make_array(dims1[2]+1,dims1[4]+1,1,type = 4)
;;  ENF
;    Tmin_open(where(landcover eq 221)) = M_Tmin_open(0)
;    Tmin_close(where(landcover eq 221))= M_Tmin_close(0)
;    VPD_close(where(landcover eq 221)) = M_VPD_close(0)
;    VPD_open(where(landcover eq 221))  = M_VPD_open(0)
;    gl_sh(where(landcover eq 221))     = M_gl_sh(0)
;    gl_e_wv(where(landcover eq 221))   = M_gl_e_wv(0)
;    Cl(where(landcover eq 221))        = M_Cl(0)
;    RBL_MIN(where(landcover eq 221))   = M_RBL_MIN(0)
;    RBL_MAX(where(landcover eq 221))   = M_RBL_MAX(0)
;;  EBF
;    Tmin_open(where(landcover eq 211)) = M_Tmin_open(1)
;    Tmin_close(where(landcover eq 211))= M_Tmin_close(1)
;    VPD_close(where(landcover eq 211)) = M_VPD_close(1)
;    VPD_open(where(landcover eq 211))  = M_VPD_open(1)
;    gl_sh(where(landcover eq 211))     = M_gl_sh(1)
;    gl_e_wv(where(landcover eq 211))   = M_gl_e_wv(1)
;    Cl(where(landcover eq 211))        = M_Cl(1)
;    RBL_MIN(where(landcover eq 211))   = M_RBL_MIN(1)
;    RBL_MAX(where(landcover eq 211))   = M_RBL_MAX(1)
;;  DNF
;    Tmin_open(where(landcover eq 222)) = M_Tmin_open(2)
;    Tmin_close(where(landcover eq 222))= M_Tmin_close(2)
;    VPD_close(where(landcover eq 222)) = M_VPD_close(2)
;    VPD_open(where(landcover eq 222))  = M_VPD_open(2)
;    gl_sh(where(landcover eq 222))     = M_gl_sh(2)
;    gl_e_wv(where(landcover eq 222))   = M_gl_e_wv(2)
;    Cl(where(landcover eq 222))        = M_Cl(2)
;    RBL_MIN(where(landcover eq 222))   = M_RBL_MIN(2)
;    RBL_MAX(where(landcover eq 222))   = M_RBL_MAX(2)
;;  DBF
;    Tmin_open(where(landcover eq 212)) = M_Tmin_open(3)
;    Tmin_close(where(landcover eq 212))= M_Tmin_close(3)
;    VPD_close(where(landcover eq 212)) = M_VPD_close(3)
;    VPD_open(where(landcover eq 212))  = M_VPD_open(3)
;    gl_sh(where(landcover eq 212))     = M_gl_sh(3)
;    gl_e_wv(where(landcover eq 212))   = M_gl_e_wv(3)
;    Cl(where(landcover eq 212))        = M_Cl(3)
;    RBL_MIN(where(landcover eq 212))   = M_RBL_MIN(3)
;    RBL_MAX(where(landcover eq 212))   = M_RBL_MAX(3)
;;  MF
;    Tmin_open(where(landcover eq 230)) = M_Tmin_open(4)
;    Tmin_close(where(landcover eq 230))= M_Tmin_close(4)
;    VPD_close(where(landcover eq 230)) = M_VPD_close(4)
;    VPD_open(where(landcover eq 230))  = M_VPD_open(4)
;    gl_sh(where(landcover eq 230))     = M_gl_sh(4)
;    gl_e_wv(where(landcover eq 230))   = M_gl_e_wv(4)
;    Cl(where(landcover eq 230))        = M_Cl(4)
;    RBL_MIN(where(landcover eq 230))   = M_RBL_MIN(4)
;    RBL_MAX(where(landcover eq 230))   = M_RBL_MAX(4)
;;  BBF, BAMBOO FOREST SAME AS MF
;    Tmin_open(where(landcover eq 214)) = M_Tmin_open(4)
;    Tmin_close(where(landcover eq 214))= M_Tmin_close(4)
;    VPD_close(where(landcover eq 214)) = M_VPD_close(4)
;    VPD_open(where(landcover eq 214))  = M_VPD_open(4)
;    gl_sh(where(landcover eq 214))     = M_gl_sh(4)
;    gl_e_wv(where(landcover eq 214))   = M_gl_e_wv(4)
;    Cl(where(landcover eq 214))        = M_Cl(4)
;    RBL_MIN(where(landcover eq 214))   = M_RBL_MIN(4)
;    RBL_MAX(where(landcover eq 214))   = M_RBL_MAX(4)
;  Types check https://lpdaac.usgs.gov/products/modis_products_table/mcd12q1 or C:\Users\zyu\Desktop\wue\MOD12Q1\MODlandcover.txt

;  ENF
    Tmin_open(where(landcover eq 1)) = M_Tmin_open(0)
    Tmin_close(where(landcover eq 1))= M_Tmin_close(0)
    VPD_close(where(landcover eq 1)) = M_VPD_close(0)
    VPD_open(where(landcover eq 1))  = M_VPD_open(0)
    gl_sh(where(landcover eq 1))     = M_gl_sh(0)
    gl_e_wv(where(landcover eq 1))   = M_gl_e_wv(0)
    Cl(where(landcover eq 1))        = M_Cl(0)
    RBL_MIN(where(landcover eq 1))   = M_RBL_MIN(0)
    RBL_MAX(where(landcover eq 1))   = M_RBL_MAX(0)
;  EBF
    Tmin_open(where(landcover eq 2)) = M_Tmin_open(1)
    Tmin_close(where(landcover eq 2))= M_Tmin_close(1)
    VPD_close(where(landcover eq 2)) = M_VPD_close(1)
    VPD_open(where(landcover eq 2))  = M_VPD_open(1)
    gl_sh(where(landcover eq 2))     = M_gl_sh(1)
    gl_e_wv(where(landcover eq 2))   = M_gl_e_wv(1)
    Cl(where(landcover eq 2))        = M_Cl(1)
    RBL_MIN(where(landcover eq 2))   = M_RBL_MIN(1)
    RBL_MAX(where(landcover eq 2))   = M_RBL_MAX(1)
;  DNF
    Tmin_open(where(landcover eq 3)) = M_Tmin_open(2)
    Tmin_close(where(landcover eq 3))= M_Tmin_close(2)
    VPD_close(where(landcover eq 3)) = M_VPD_close(2)
    VPD_open(where(landcover eq 3))  = M_VPD_open(2)
    gl_sh(where(landcover eq 3))     = M_gl_sh(2)
    gl_e_wv(where(landcover eq 3))   = M_gl_e_wv(2)
    Cl(where(landcover eq 3))        = M_Cl(2)
    RBL_MIN(where(landcover eq 3))   = M_RBL_MIN(2)
    RBL_MAX(where(landcover eq 3))   = M_RBL_MAX(2)
;  DBF
    Tmin_open(where(landcover eq 4)) = M_Tmin_open(3)
    Tmin_close(where(landcover eq 4))= M_Tmin_close(3)
    VPD_close(where(landcover eq 4)) = M_VPD_close(3)
    VPD_open(where(landcover eq 4))  = M_VPD_open(3)
    gl_sh(where(landcover eq 4))     = M_gl_sh(3)
    gl_e_wv(where(landcover eq 4))   = M_gl_e_wv(3)
    Cl(where(landcover eq 4))        = M_Cl(3)
    RBL_MIN(where(landcover eq 4))   = M_RBL_MIN(3)
    RBL_MAX(where(landcover eq 4))   = M_RBL_MAX(3)
;  MF
    Tmin_open(where(landcover eq 5)) = M_Tmin_open(4)
    Tmin_close(where(landcover eq 5))= M_Tmin_close(4)
    VPD_close(where(landcover eq 5)) = M_VPD_close(4)
    VPD_open(where(landcover eq 5))  = M_VPD_open(4)
    gl_sh(where(landcover eq 5))     = M_gl_sh(4)
    gl_e_wv(where(landcover eq 5))   = M_gl_e_wv(4)
    Cl(where(landcover eq 5))        = M_Cl(4)
    RBL_MIN(where(landcover eq 5))   = M_RBL_MIN(4)
    RBL_MAX(where(landcover eq 5))   = M_RBL_MAX(4)
;  CSH
    Tmin_open(where(landcover eq 6)) = M_Tmin_open(5)
    Tmin_close(where(landcover eq 6))= M_Tmin_close(5)
    VPD_close(where(landcover eq 6)) = M_VPD_close(5)
    VPD_open(where(landcover eq 6))  = M_VPD_open(5)
    gl_sh(where(landcover eq 6))     = M_gl_sh(5)
    gl_e_wv(where(landcover eq 6))   = M_gl_e_wv(5)
    Cl(where(landcover eq 6))        = M_Cl(5)
    RBL_MIN(where(landcover eq 6))   = M_RBL_MIN(5)
    RBL_MAX(where(landcover eq 6))   = M_RBL_MAX(5)
;  OSH   
    Tmin_open(where(landcover eq 7)) = M_Tmin_open(6)
    Tmin_close(where(landcover eq 7))= M_Tmin_close(6)
    VPD_close(where(landcover eq 7)) = M_VPD_close(6)
    VPD_open(where(landcover eq 7))  = M_VPD_open(6)
    gl_sh(where(landcover eq 7))     = M_gl_sh(6)
    gl_e_wv(where(landcover eq 7))   = M_gl_e_wv(6)
    Cl(where(landcover eq 7))        = M_Cl(6)
    RBL_MIN(where(landcover eq 7))   = M_RBL_MIN(6)
    RBL_MAX(where(landcover eq 7))   = M_RBL_MAX(6)
;  WL 
    Tmin_open(where(landcover eq 8)) = M_Tmin_open(7)
    Tmin_close(where(landcover eq 8))= M_Tmin_close(7)
    VPD_close(where(landcover eq 8)) = M_VPD_close(7)
    VPD_open(where(landcover eq 8))  = M_VPD_open(7)
    gl_sh(where(landcover eq 8))     = M_gl_sh(7)
    gl_e_wv(where(landcover eq 8))   = M_gl_e_wv(7)
    Cl(where(landcover eq 8))        = M_Cl(7)
    RBL_MIN(where(landcover eq 8))   = M_RBL_MIN(7)
    RBL_MAX(where(landcover eq 8))   = M_RBL_MAX(7)
;  SV   
    Tmin_open(where(landcover eq 9)) = M_Tmin_open(8)
    Tmin_close(where(landcover eq 9))= M_Tmin_close(8)
    VPD_close(where(landcover eq 9)) = M_VPD_close(8)
    VPD_open(where(landcover eq 9))  = M_VPD_open(8)
    gl_sh(where(landcover eq 9))     = M_gl_sh(8)
    gl_e_wv(where(landcover eq 9))   = M_gl_e_wv(8)
    Cl(where(landcover eq 9))        = M_Cl(8)
    RBL_MIN(where(landcover eq 9))   = M_RBL_MIN(8)
    RBL_MAX(where(landcover eq 9))   = M_RBL_MAX(8)
;  GRASS 
    Tmin_open(where(landcover eq 10)) = M_Tmin_open(9)
    Tmin_close(where(landcover eq 10))= M_Tmin_close(9)
    VPD_close(where(landcover eq 10)) = M_VPD_close(9)
    VPD_open(where(landcover eq 10))  = M_VPD_open(9)
    gl_sh(where(landcover eq 10))     = M_gl_sh(9)
    gl_e_wv(where(landcover eq 10))   = M_gl_e_wv(9)
    Cl(where(landcover eq 10))        = M_Cl(9)
    RBL_MIN(where(landcover eq 10))   = M_RBL_MIN(9)
    RBL_MAX(where(landcover eq 10))   = M_RBL_MAX(9)
;  CROP
    Tmin_open(where(landcover eq 12)) = M_Tmin_open(10)
    Tmin_close(where(landcover eq 12))= M_Tmin_close(10)
    VPD_close(where(landcover eq 12)) = M_VPD_close(10)
    VPD_open(where(landcover eq 12))  = M_VPD_open(10)
    gl_sh(where(landcover eq 12))     = M_gl_sh(10)
    gl_e_wv(where(landcover eq 12))   = M_gl_e_wv(10)
    Cl(where(landcover eq 12))        = M_Cl(10)
    RBL_MIN(where(landcover eq 12))   = M_RBL_MIN(10)
    RBL_MAX(where(landcover eq 12))   = M_RBL_MAX(10)
    
    
  Slon =  fltarr(dims1[2]+1,dims1[4]+1)
  Slat =  fltarr(dims1[2]+1,dims1[4]+1)
  for i=0,dims1[4] do begin
    Slon[*,i] = xstart + indgen(dims1[2]+1)*0.5 ;
  endfor
  for i=0,dims1[2] do begin
    Slat[i,*] = ystart - indgen(dims1[4]+1)*0.5 ;
  endfor
  
  TrOUT = fltarr(dims1[2]+1,dims1[4]+1,nb1)
  ETOUT = fltarr(dims1[2]+1,dims1[4]+1,nb1)

For i = 0, nb1-1 do begin
  print, nb1-i
        lai    = envi_get_data(fid = fid0,dims = dims1, pos = i);*0.1
        fpar   = envi_get_data(fid = fid1,dims = dims1, pos = i)
        albedo = envi_get_data(fid = fid2,dims = dims1, pos = i)*0.0001
        tair   = envi_get_data(fid = fid3,dims = dims1, pos = i)
        tmax   = envi_get_data(fid = fid4,dims = dims1, pos = i)
        tmin   = envi_get_data(fid = fid5,dims = dims1, pos = i)
        RH     = envi_get_data(fid = fid6,dims = dims1, pos = i)
;        SSD    = envi_get_data(fid = fid7,dims = dims1, pos = i)
        Rs     = envi_get_data(fid = fid10,dims = dims1, pos = i)
        ndvi   = envi_get_data(fid = fid8,dims = dims1, pos = i)
        
;        T_annual= envi_get_data(fid = fid8,dims = dims1, pos = (i/46))
        
        Fc        = FPAR
        day       = (i mod 46)*8+1
    ;    Tnight   = 2.0*Tavg-Tday
        Tday     = (Tair+Tmax)/2
        Tnight   = (Tair+Tmin)/2
        
        e_out = FLTARR(dims1[2]+1,dims1[4]+1,4+4); first 4 for et; last 4 for pet
        ;pe_out = make_array(dims1[2]+1,dims1[4]+1,4,type = type1)
    ;-------------------------------------------------------------------
    ; 1. FOR DAY
        FLAGdaynight = 1
        Tin          = Tday
        et_out_day = ET(e_out, FLAGdaynight, slat, day, Tin, Tmin, Fc, ndvi, Tday, Tnight, RH, elev, Pa, albedo, LAI, $
                        Tmin_open, Tmin_close, VPD_close, VPD_open, gl_sh, gl_e_wv, Cl, RBL_MIN, RBL_MAX, Rs)
        ; mm/s
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,0], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-evap-canopy-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,1], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-evap-soil-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,2], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-transp-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,3], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-ET-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,4], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Wetsoil-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,5], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Soilpot-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,6], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Tranpot-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,7], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-'+name0(i),map_info=map_info
   ;-------------------------------------------------------------------
    ; 2. FOR NIGHT
        FLAGdaynight = 0
        Tin          = Tnight
        et_out_night = ET(e_out, FLAGdaynight, slat, day, Tin, Tmin, Fc, ndvi, Tday, Tnight, RH, elev, Pa, albedo, LAI, $
                        Tmin_open, Tmin_close, VPD_close, VPD_open, gl_sh, gl_e_wv, Cl, RBL_MIN, RBL_MAX, Rs)    
        ; mm/s
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,0], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-evap-canopy-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,1], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-evap-soil-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,2], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-transp-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,3], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-ET-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,4], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-PET-Wetsoil-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,5], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-PET-Soilpot-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,6], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-PET-Tranpot-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_night[*,*,7], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\night-LCC-PET-'+name0(i),map_info=map_info


;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,0]+et_out_night[*,*,0], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC-new\LCC-evap-canopy-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,1]+et_out_night[*,*,1], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC-new\LCC-evap-soil-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,2]+et_out_night[*,*,2], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC-new\LCC-transp-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,3]+et_out_night[*,*,3], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC-new\LCC-ET-'+name0(i),map_info=map_info
;        ;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,4], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Wetsoil-'+name0(i),map_info=map_info
;        ;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,5], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Soilpot-'+name0(i),map_info=map_info
;        ;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,6], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC\day-LCC-PET-Tranpot-'+name0(i),map_info=map_info
;        ENVI_WRITE_ENVI_FILE, et_out_day[*,*,7]+et_out_night[*,*,7], out_name='C:\Users\zyu\Desktop\wue\modeloutput\LCC-new\LCC-PET-'+name0(i),map_info=map_info
        
        TrOUT[*,*,i] = et_out_day[*,*,2]+et_out_night[*,*,2]
        ETOUT[*,*,i] = et_out_day[*,*,3]+et_out_night[*,*,3]
        
  endfor
        ENVI_WRITE_ENVI_FILE, TrOUT,out_name='C:\Users\zyu\Desktop\Chapter4\ET\LCC-newlai-transp',map_info=map_info
        ENVI_WRITE_ENVI_FILE, ETOUT,out_name='C:\Users\zyu\Desktop\Chapter4\ET\LCC-newlai-ET'    ,map_info=map_info
  end