        !COMPILER-GENERATED INTERFACE MODULE: Fri May 10 10:31:45 2013
        MODULE GETINPUT__genmod
          INTERFACE 
            SUBROUTINE GETINPUT(IFLUXOUT,ISOLARCALC,IRCALC,ISLOPE,      &
     &ITRACKS,ITM,IOUTFILTRATE,IMETCALC,NGOODMIN,ITIMEZONE,IY2,JDAY2,   &
     &IHOUR2,PINV,BP,BEXT,ALBSNOW,HEIGHT,DTMIN,DTSMIN,DTMAX,DTSSMAX,    &
     &DSSALLOWED,ERRTALLOWD,EMSNOW,DLATT,ELEV,DLONGT,AZSLOPE,DZMIN,DZN, &
     &DZNM,FNM,SSISNOW,FRH,BIFALLIN,DMLIMIT,ISTBOFF,IQTURB,ETA0,EXP1)
              INTEGER(KIND=4) :: IFLUXOUT
              INTEGER(KIND=4) :: ISOLARCALC
              INTEGER(KIND=4) :: IRCALC
              INTEGER(KIND=4) :: ISLOPE
              INTEGER(KIND=4) :: ITRACKS
              INTEGER(KIND=4) :: ITM
              INTEGER(KIND=4) :: IOUTFILTRATE
              INTEGER(KIND=4) :: IMETCALC
              INTEGER(KIND=4) :: NGOODMIN
              INTEGER(KIND=4) :: ITIMEZONE
              INTEGER(KIND=4) :: IY2
              INTEGER(KIND=4) :: JDAY2
              INTEGER(KIND=4) :: IHOUR2
              REAL(KIND=8) :: PINV
              REAL(KIND=8) :: BP
              REAL(KIND=8) :: BEXT
              REAL(KIND=8) :: ALBSNOW
              REAL(KIND=8) :: HEIGHT(3)
              REAL(KIND=8) :: DTMIN
              REAL(KIND=8) :: DTSMIN
              REAL(KIND=8) :: DTMAX
              REAL(KIND=8) :: DTSSMAX
              REAL(KIND=8) :: DSSALLOWED
              REAL(KIND=8) :: ERRTALLOWD
              REAL(KIND=8) :: EMSNOW
              REAL(KIND=8) :: DLATT
              REAL(KIND=8) :: ELEV
              REAL(KIND=8) :: DLONGT
              REAL(KIND=8) :: AZSLOPE
              REAL(KIND=8) :: DZMIN
              REAL(KIND=8) :: DZN
              REAL(KIND=8) :: DZNM
              CHARACTER(LEN=160) :: FNM(6)
              REAL(KIND=8) :: SSISNOW
              REAL(KIND=8) :: FRH(5)
              REAL(KIND=8) :: BIFALLIN
              REAL(KIND=8) :: DMLIMIT
              INTEGER(KIND=4) :: ISTBOFF
              INTEGER(KIND=4) :: IQTURB
              REAL(KIND=8) :: ETA0
              REAL(KIND=8) :: EXP1
            END SUBROUTINE GETINPUT
          END INTERFACE 
        END MODULE GETINPUT__genmod
