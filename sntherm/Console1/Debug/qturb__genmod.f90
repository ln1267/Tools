        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:38 2013
        MODULE QTURB__genmod
          INTERFACE 
            SUBROUTINE QTURB(HEIGHT,TKAIR,TSURFACE,EAIR,ESURFACE,WSP,   &
     &WSPO,QLAT,QSEN,DLIQVOL,CDRYAIR,RW,CK,CSK,SNOWDEPTH,LTYPE,Z0,CDN,  &
     &RHOAIR,BPRESS,DLOGT,DLOGQ,DLOGW)
              REAL(KIND=8) :: HEIGHT(3)
              REAL(KIND=8) :: TKAIR
              REAL(KIND=8) :: TSURFACE
              REAL(KIND=8) :: EAIR
              REAL(KIND=8) :: ESURFACE
              REAL(KIND=8) :: WSP
              REAL(KIND=8) :: WSPO
              REAL(KIND=8) :: QLAT
              REAL(KIND=8) :: QSEN
              REAL(KIND=8) :: DLIQVOL
              REAL(KIND=8) :: CDRYAIR
              REAL(KIND=8) :: RW
              REAL(KIND=8) :: CK
              REAL(KIND=8) :: CSK
              REAL(KIND=8) :: SNOWDEPTH
              INTEGER(KIND=4) :: LTYPE
              REAL(KIND=8) :: Z0
              REAL(KIND=8) :: CDN
              REAL(KIND=8) :: RHOAIR
              REAL(KIND=8) :: BPRESS
              REAL(KIND=8) :: DLOGT
              REAL(KIND=8) :: DLOGQ
              REAL(KIND=8) :: DLOGW
            END SUBROUTINE QTURB
          END INTERFACE 
        END MODULE QTURB__genmod
