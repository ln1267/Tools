Pro Readhdf1


;template = HDF_BROWSER('L:\linshi\zip\SV04_VG1_S10_VI_19990110\0001\0001_NDV.hdf') 
;output_structure = HDF_READ(TEMPLATE=template)
;
;output_structure = HDF_READ('L:\linshi\zip\SV04_VG1_S10_VI_19990110\0001\0001_NDV.hdf')
;
;; Select'my.hdf' with the file locator
;output_structure = HDF_READ()
;     
;output_structure = HDF_READ('just_like_my.hdf', TEMPLATE=template)
Result = HDF_READ( 'L:\linshi\zip\SV04_VG1_S10_VI_19990110\0001\0001_NDV.hdf', DFR8= 'PIXEL DATA')








END