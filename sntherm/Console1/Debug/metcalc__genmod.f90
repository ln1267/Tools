        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:45 2013
        MODULE METCALC__genmod
          INTERFACE 
            SUBROUTINE METCALC(JDAY,IHOUR,IMIN,TKAIR,RH,WSP,PRCP,IPTYPE,&
     &DSNOWFALL,COVER,ICL,INITIAL,DTBASE,SHADOW)
              INTEGER(KIND=4) :: JDAY
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: IMIN
              REAL(KIND=8) :: TKAIR
              REAL(KIND=8) :: RH
              REAL(KIND=8) :: WSP
              REAL(KIND=8) :: PRCP
              INTEGER(KIND=4) :: IPTYPE
              REAL(KIND=8) :: DSNOWFALL
              REAL(KIND=8) :: COVER(3)
              INTEGER(KIND=4) :: ICL(3)
              INTEGER(KIND=4) :: INITIAL
              REAL(KIND=8) :: DTBASE
              REAL(KIND=8) :: SHADOW
            END SUBROUTINE METCALC
          END INTERFACE 
        END MODULE METCALC__genmod
