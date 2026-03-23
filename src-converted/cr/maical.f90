SUBROUTINE MAICAL
IMPLICIT NONE
!----------
! CR $Id$
!----------
!  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
!  FROM CRATET.
!----------
!
!OMMONS
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'COEFFS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'OUTCOM.f90'
!
!
INCLUDE 'HTCAL.f90'
!
!
!OMMONS
!
!----------
LOGICAL DEBUG
REAL ADJMAI,SSSI
INTEGER ISPNUM(11),IMAP,ISICD,IERR
INTEGER MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP), &
             MAP5(MAXSP)
!----------
!  SPECIES ORDER:
!   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
!  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
!  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
!  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
!
!  SPECIES EXPANSION:
!  UJ,AJ,RM,OJ,ER USE CR JU
!  NC,PW USE CR CO
!  GO,AW,EM,BK,SO USE CR OA
!  PB USES CR AS
!  PM,PD,AZ USE CR PI
!  CI USES CR PP
!----------
!
DATA MAP1/ &
     9,  9,  3,  9,  4,  2,  2,  2,  1,  1, &
     2, 11, 10,  1,  1, 11,  7,  8,  8,  6, &
     5,  5,  5,  5,  5,  5,  5,  6, 11, 11, &
    11, 11, 11, 11, 11, 10,  2,  5/
DATA MAP2/ &
     4,  4,  3,  4,  4,  2,  2,  2,  1,  1, &
     2,  9, 10,  1,  1, 11,  7,  7,  7,  6, &
     5,  5,  8,  8,  8,  8,  8,  6, 11, 11, &
    11, 11,  9,  9,  9, 10,  2,  5/
DATA MAP3/ &
     2,  2,  3,  2,  2,  2,  2,  2,  2,  2, &
     7,  2, 10,  2,  2, 11,  8,  8,  8,  6, &
     1,  1,  5,  5,  5,  5,  5,  6, 11, 11, &
    11, 11,  2,  2,  2, 10,  2,  5/
DATA MAP4/ &
     9,  9,  3,  9,  9,  2,  2,  2,  2,  2, &
     7,  2,  2,  2,  2,  2,  8,  8,  8,  6, &
     5,  5,  5,  5,  5,  5,  5,  6,  2,  2, &
     2,  2,  2,  2,  2,  2,  2,  5/
DATA MAP5/ &
     9,  9,  3,  9,  9,  2,  2,  2,  2,  2, &
     7,  2, 10,  2,  2,  2,  8,  8,  8,  6, &
     5,  5,  5,  5,  5,  5,  5,  6,  2,  2, &
     2,  2,  2,  2,  2, 10,  2,  5/
!----------
!  INITIALIZE INTERNAL VARIABLES:
!----------
DATA ISPNUM/101,101,202,015,101,101,108,093,019,122,101/
!
!     THE SPECIES ORDER IS AS FOLLOWS:
!     1 = WHITE BARK PINE (WB)
!     2 = LIMBER PINE (LM) USE WB
!     3 = DOUGLAS-FIR (DF)
!     4 = WHITE FIR (WF)
!     5 = DUMMY
!     6 = ASPEN (AS) USE WB
!     7 = LODGEPOLE PINE (LP)
!     8 = ENGLEMAN SPRUCE (ES)
!     9 = SUBALPINE FIR (AF)
!    10 = PONDEROSA PINE (PP)
!    11 = OTHER ()
!
!
!-----------
!  SEE IF WE NEED TO DO SOME DEBUG.
!-----------
CALL DBCHK (DEBUG,'MAICAL',6,ICYC)
!
IF(DEBUG) WRITE(JOSTND,3)ICYC
3 FORMAT(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)
!
IF (ISISP .EQ. 0) ISISP=3
SSSI=SITEAR(ISISP)
IF (SSSI .EQ. 0.) SSSI=140.0
!----------
!  SET UP MAPPING INDEX BY  MODEL TYPE.
!----------
IF(IMODTY .EQ. 1) THEN
  IMAP=MAP1(ISISP)
ELSEIF(IMODTY .EQ. 2) THEN
  IMAP=MAP2(ISISP)
ELSEIF(IMODTY .EQ. 3) THEN
  IMAP=MAP3(ISISP)
ELSEIF(IMODTY .EQ. 4) THEN
  IMAP=MAP4(ISISP)
ELSE
  IMAP=MAP5(ISISP)
ENDIF
!-------
!   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.
!-------
ISICD=ISPNUM(IMAP)
RMAI=ADJMAI(ISICD,SSSI,10.0,IERR)
IF(RMAI .GT. 128.0)RMAI=128.0
RETURN
END
