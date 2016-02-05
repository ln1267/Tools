        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:51 2013
        MODULE SLOPE__genmod
          INTERFACE 
            SUBROUTINE SLOPE(SINZ,COSZ,RELAZIMUTH,COSELV,SDOWN,SALB,    &
     &DIFFUSESLOPE,DIRECTSLOPE,COVER,ICL,ECOVER2,ECOVER3,CLEARNESS,     &
     &DIRECT,DIFFUSE,SINELV,COSAZI,REFLECTED,HR,AZSLOPE,COSD,ELEV,DLATT,&
     &SIND)
              REAL(KIND=8) :: SINZ
              REAL(KIND=8) :: COSZ
              REAL(KIND=8) :: RELAZIMUTH
              REAL(KIND=8) :: COSELV
              REAL(KIND=8) :: SDOWN
              REAL(KIND=8) :: SALB
              REAL(KIND=8) :: DIFFUSESLOPE
              REAL(KIND=8) :: DIRECTSLOPE
              REAL(KIND=8) :: COVER(3)
              INTEGER(KIND=4) :: ICL(3)
              REAL(KIND=8) :: ECOVER2
              REAL(KIND=8) :: ECOVER3
              REAL(KIND=8) :: CLEARNESS
              REAL(KIND=8) :: DIRECT
              REAL(KIND=8) :: DIFFUSE
              REAL(KIND=8) :: SINELV
              REAL(KIND=8) :: COSAZI
              REAL(KIND=8) :: REFLECTED
              REAL(KIND=8) :: HR
              REAL(KIND=8) :: AZSLOPE
              REAL(KIND=8) :: COSD
              REAL(KIND=8) :: ELEV
              REAL(KIND=8) :: DLATT
              REAL(KIND=8) :: SIND
            END SUBROUTINE SLOPE
          END INTERFACE 
        END MODULE SLOPE__genmod
