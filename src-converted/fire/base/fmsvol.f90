SUBROUTINE FMSVOL (II, XHT, VOL2HT, DEBUG, IOUT)
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!     CALLED FROM: FMSOUT
!                  FMDOUT
!                  FMHIDE
!                  FMCWD
!                  FMSALV
!
!     CALLS:       CFVOL
!                  NATCRS
!                  OCFVOL
!                  CFTOPK
!
!  PURPOSE:
!     Calculates the volume up to height HT of each snag in record II.
!----------------------------------------------------------------------
!
!  CALL LIST DEFINITIONS:
!     II       SNAG RECORD NUMBER
!     XHT      HEIGHT UP TO WHICH VOLUME SHOULD BE CALCULATED
!     VOL2HT   VOLUME UP TO HEIGHT HT OF EACH SNAG IN RECORD II
!     MVOL     MERCH VOLUMNE UP TO HEIGHT
!
!  LOCAL VARIABLE DEFINITIONS:
!     MCF = MERCH. VOLUME
!     JS = SNAG SPECIES
!     IHT = AN INDEX OF HT
!
!  COMMON BLOCK VARIABLES AND PARAMETERS:
!
!**********************************************************************

!.... PARAMETER STATEMENTS.

!.... PARAMETER INCLUDE FILES.

INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... COMMON INCLUDE FILES.

INCLUDE 'FMCOM.f90'
INCLUDE 'CONTRL.f90'

!.... VARIABLE DECLARATIONS.

REAL     MCF, VMAX, BFMAX, TCF, SCF
LOGICAL  LTKIL, LC, LCONE, CTKFLG, BTKFLG, DEBUG
LOGICAL  LMERCHIN, LMERCH
INTEGER  JS, ISPC, IT
REAL     D, H, BARK, XHT, VOL2HT
INTEGER  IOUT,II,JSP,IHT
INTEGER  CRWNRTO,CWN,DECAY,DCY,WDSTMS,WSTM
REAL     XH,XD,BRATIO,D2H,BBFV,X,CUL,CL,BIODRY(15)
CHARACTER LIVEDEAD,LVD

!     CALCULATE THE VOLUME

JS = SPS(II)
D = DBHS(II)
H = HTDEAD(II)
LMERCH = .FALSE.
CWN = 0
CL = 0
DCY = 0
WSTM = 0
IF(LFIANVB) THEN
  LVD = 'D'
ELSE
  LVD = ' '
ENDIF

GOTO 1000

!     ENTRY POINT FOR SNAGS CREATED BY **CUTS**.

ENTRY FMSVL2(JSP,XD,XH,XHT,VOL2HT,CRWNRTO, &
                LIVEDEAD,LMERCHIN,DEBUG,IOUT)

JS = JSP
D  = XD
H  = XH
LMERCH = LMERCHIN

CWN = CRWNRTO
CL     = 0
DCY    = 0
WSTM   = 0
BIODRY = 0
IF(LFIANVB) THEN
  LVD = LIVEDEAD
ELSE
  LVD = ' '
ENDIF

1000 CONTINUE

LC    = .FALSE.
IF (XHT .GT. -1) THEN
  LTKIL = .TRUE.
ELSE
  LTKIL = .FALSE.
  XHT = H
ENDIF

BARK = BRATIO(JS,D,H)
D2H = D * D * H
IHT = INT(XHT * 100.0)

!      This call was replaced by the code below (NATCRS, etc.).
!      CALL CFVOL (JS,D,H,D2H,VOL2HT,VM,VMAX,LTKIL,LC,BARK,IHT,LDUM)

!     Actually do the call to calculate the volumes. Note that this
!     section of the code may have to change if there are any changes
!     made within the base model routine VOLS or CFVOL
!     Also note the following variable equivalencies:
!     ITRUNC(I) = IHT
!     TKILL = LTKIL

ISPC   = JS
LCONE  = LC
IT     = 0
CTKFLG = LTKIL
BTKFLG = .FALSE.

IF(METHC(ISPC).EQ.6 .OR. METHC(ISPC).EQ.10) THEN
   CALL NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,LTKIL, &
                   CWN,BARK,IHT,VMAX,BFMAX, &
                   CL,DCY,WSTM,BIODRY, &
                   LVD,CTKFLG,BTKFLG,-1)
ELSEIF ((METHC(ISPC).EQ.8).OR.(METHC(ISPC).EQ.5)) THEN
   CALL OCFVOL (TCF,MCF,ISPC,D,H,LTKIL,BARK,IHT,VMAX,LCONE, &
           CTKFLG,IT)
ELSE
   CALL CFVOL (ISPC,D,H,D2H,TCF,MCF,VMAX,LTKIL,LCONE,BARK,IHT, &
           CTKFLG)
ENDIF
IF(CTKFLG .AND. LTKIL) &
        CALL CFTOPK (ISPC,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,IHT)

!     Give some small volume to very tiny trees.
!     based on cone with D = 1 inch
X = 0.005454154 * H

IF (VARACD .EQ. 'CS' .OR. VARACD .EQ. 'LS' &
       .OR. VARACD .EQ. 'NE' .OR. VARACD .EQ. 'SN') THEN
  VOL2HT = MAX(X,MCF)
  IF (LMERCH) VOL2HT = SCF
ELSE
  VOL2HT = MAX(X,TCF)
  IF (LMERCH) VOL2HT = MCF
ENDIF

IF (DEBUG) WRITE(IOUT,40)ISPC,D,H,LCONE,TCF,VOL2HT
40 FORMAT(' FMSVOL ISPC=',I3,' D=',F7.3,' H=',F7.3, &
        ' LCONE=',L2,' VN=',F7.3,' VOL2HT=',F10.3)

RETURN
END

