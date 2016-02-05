        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:40 2013
        MODULE SDSOL__genmod
          INTERFACE 
            SUBROUTINE SDSOL(DSOL,D,DMASS,FEXT,DEPTH,NSOIL,N,SOLAR,BEXT)
              REAL(KIND=8) :: DSOL(200)
              REAL(KIND=8) :: D(200)
              REAL(KIND=8) :: DMASS(200)
              REAL(KIND=8) :: FEXT(200)
              REAL(KIND=8) :: DEPTH
              INTEGER(KIND=4) :: NSOIL
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: SOLAR
              REAL(KIND=8) :: BEXT
            END SUBROUTINE SDSOL
          END INTERFACE 
        END MODULE SDSOL__genmod
