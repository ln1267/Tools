        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:36 2013
        MODULE SKYRAD__genmod
          INTERFACE 
            SUBROUTINE SKYRAD(COVER,TKAIR,EA,CEILING,ERRSKY,CLEARNESS,  &
     &NMAX,ECOVER2,ECOVER3)
              REAL(KIND=8) :: COVER(3)
              REAL(KIND=8) :: TKAIR
              REAL(KIND=8) :: EA
              REAL(KIND=8) :: CEILING(3)
              REAL(KIND=8) :: ERRSKY
              REAL(KIND=8) :: CLEARNESS
              INTEGER(KIND=4) :: NMAX
              REAL(KIND=8) :: ECOVER2
              REAL(KIND=8) :: ECOVER3
            END SUBROUTINE SKYRAD
          END INTERFACE 
        END MODULE SKYRAD__genmod
