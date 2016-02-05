        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:47 2013
        MODULE FLUX__genmod
          INTERFACE 
            SUBROUTINE FLUX(JDAY,IHOUR,IBASESTEP,IPTYPE,HEIGHT,IY,      &
     &CLEARNESS,IMIN,ISOLARCALC,EFFCEILING,ISLOPE,FFMT)
              INTEGER(KIND=4) :: JDAY
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: IBASESTEP
              INTEGER(KIND=4) :: IPTYPE
              REAL(KIND=8) :: HEIGHT(3)
              INTEGER(KIND=4) :: IY
              REAL(KIND=8) :: CLEARNESS
              INTEGER(KIND=4) :: IMIN
              INTEGER(KIND=4) :: ISOLARCALC
              REAL(KIND=8) :: EFFCEILING
              INTEGER(KIND=4) :: ISLOPE
              INTEGER(KIND=4) :: FFMT
            END SUBROUTINE FLUX
          END INTERFACE 
        END MODULE FLUX__genmod
