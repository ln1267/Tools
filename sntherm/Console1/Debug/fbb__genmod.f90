        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:47 2013
        MODULE FBB__genmod
          INTERFACE 
            SUBROUTINE FBB(QK,T,DSOL,QF,TOPFLUX,BB,N,HEATFLUXBTOP,NSOIL)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: QK(N)
              REAL(KIND=8) :: T(N)
              REAL(KIND=8) :: DSOL(N)
              REAL(KIND=8) :: QF(N)
              REAL(KIND=8) :: TOPFLUX
              REAL(KIND=8) :: BB(N)
              REAL(KIND=8) :: HEATFLUXBTOP
              INTEGER(KIND=4) :: NSOIL
            END SUBROUTINE FBB
          END INTERFACE 
        END MODULE FBB__genmod
