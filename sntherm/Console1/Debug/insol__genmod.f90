        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:51 2013
        MODULE INSOL__genmod
          INTERFACE 
            SUBROUTINE INSOL(DLATT,DLONGT,JDAY,COVER,ICL,PRCP,SALB,GMT, &
     &SDOWN,COSZ,SIND,COSD,XM,SDOWNE,IDO,DIRECT,DIFFUSE,HR,ISOLARCALC)
              REAL(KIND=8) :: DLATT
              REAL(KIND=8) :: DLONGT
              INTEGER(KIND=4) :: JDAY
              REAL(KIND=8) :: COVER(3)
              INTEGER(KIND=4) :: ICL(3)
              REAL(KIND=8) :: PRCP
              REAL(KIND=8) :: SALB
              REAL(KIND=8) :: GMT
              REAL(KIND=8) :: SDOWN
              REAL(KIND=8) :: COSZ
              REAL(KIND=8) :: SIND
              REAL(KIND=8) :: COSD
              REAL(KIND=8) :: XM
              REAL(KIND=8) :: SDOWNE
              INTEGER(KIND=4) :: IDO
              REAL(KIND=8) :: DIRECT
              REAL(KIND=8) :: DIFFUSE
              REAL(KIND=8) :: HR
              INTEGER(KIND=4) :: ISOLARCALC
            END SUBROUTINE INSOL
          END INTERFACE 
        END MODULE INSOL__genmod
