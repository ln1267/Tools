        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:48 2013
        MODULE CONVERGE__genmod
          INTERFACE 
            SUBROUTINE CONVERGE(REPEAT,IGOOD,NGOODMIN,DTSUM,DSSALLOWED, &
     &ERRTALLOWD,DTMIN,DTMAX,DTSMIN,DTSSMAX,PRINT)
              LOGICAL(KIND=4) :: REPEAT
              INTEGER(KIND=4) :: IGOOD
              INTEGER(KIND=4) :: NGOODMIN
              REAL(KIND=8) :: DTSUM
              REAL(KIND=8) :: DSSALLOWED
              REAL(KIND=8) :: ERRTALLOWD
              REAL(KIND=8) :: DTMIN
              REAL(KIND=8) :: DTMAX
              REAL(KIND=8) :: DTSMIN
              REAL(KIND=8) :: DTSSMAX
              LOGICAL(KIND=4) :: PRINT
            END SUBROUTINE CONVERGE
          END INTERFACE 
        END MODULE CONVERGE__genmod
