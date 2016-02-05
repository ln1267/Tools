        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 09:47:40 2013
        MODULE WRITE__genmod
          INTERFACE 
            SUBROUTINE WRITE(PINV,IBASESTEP,IHOUR,IY,JDAY,TMN,DE0,BP,   &
     &DIFFTEMP,RTMSQ,ICALCSTEP,FNM,TMSG,ITM,HEIGHT,BEXT,DTMIN,DTMAX,    &
     &DTSMIN,DTSSMAX,DSSALLOWED,ERRTALLOWD,NGOODMIN,IMIN,ALBSNOW,ISLOPE,&
     &ISOLARCALC,IRCALC,ELEV,AZSLOPE,BIFALLIN,DMLIMIT,ISTBOFF,SSISNOW,  &
     &IQTURB,ETA0)
              REAL(KIND=8) :: PINV
              INTEGER(KIND=4) :: IBASESTEP
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: IY
              INTEGER(KIND=4) :: JDAY
              REAL(KIND=8) :: TMN
              REAL(KIND=8) :: DE0
              REAL(KIND=8) :: BP
              REAL(KIND=8) :: DIFFTEMP
              REAL(KIND=8) :: RTMSQ
              INTEGER(KIND=4) :: ICALCSTEP
              CHARACTER(LEN=12) :: FNM(6)
              REAL(KIND=8) :: TMSG
              INTEGER(KIND=4) :: ITM
              REAL(KIND=8) :: HEIGHT(3)
              REAL(KIND=8) :: BEXT
              REAL(KIND=8) :: DTMIN
              REAL(KIND=8) :: DTMAX
              REAL(KIND=8) :: DTSMIN
              REAL(KIND=8) :: DTSSMAX
              REAL(KIND=8) :: DSSALLOWED
              REAL(KIND=8) :: ERRTALLOWD
              INTEGER(KIND=4) :: NGOODMIN
              INTEGER(KIND=4) :: IMIN
              REAL(KIND=8) :: ALBSNOW
              INTEGER(KIND=4) :: ISLOPE
              INTEGER(KIND=4) :: ISOLARCALC
              INTEGER(KIND=4) :: IRCALC
              REAL(KIND=8) :: ELEV
              REAL(KIND=8) :: AZSLOPE
              REAL(KIND=8) :: BIFALLIN
              REAL(KIND=8) :: DMLIMIT
              INTEGER(KIND=4) :: ISTBOFF
              REAL(KIND=8) :: SSISNOW
              INTEGER(KIND=4) :: IQTURB
              REAL(KIND=8) :: ETA0
            END SUBROUTINE WRITE
          END INTERFACE 
        END MODULE WRITE__genmod
