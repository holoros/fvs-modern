SUBROUTINE SVSTART
IMPLICIT NONE
!----------
! VBASE $Id$
!----------
!
!     STAND VISUALIZATION GENERATION
!     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
!
!     BUILD THE INITIAL DISPLAY OF THE INITIAL TREES.
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'SVDATA.f90'
!
!
INCLUDE 'SVRCOM.f90'
!
!
!OMMONS
!

LOGICAL DEBUG
INTEGER I, ID, IFC, IH, ISVOBJ, J, K, L
!
CALL DBCHK (DEBUG,'SVSTART',7,ICYC)
!
IF (JSVOUT.EQ.0) RETURN
!
!     START THE SVRANN GENERATOR AT THE SAME PLACE AS THE
!     BASE MODEL GENERATOR.  THIS MAKES THE SVRANN GENERATOR
!     RESPOND TO THE RANNSEED KEYWORD.
!
CALL RANNGET(SVS0)
!
!     SET UP THE PLOT GEOMETRY
!
CALL SVGTPL
!
NSVOBJ = 0
DO ISVOBJ=1,MXSVOB
  IOBJTP(ISVOBJ) = 0
ENDDO

!
!     PLACE THE INITIAL TREES.  NOTE THAT THIS CODE ASSUMES
!     THAT THERE ARE NO OBJECTS IN THE OBJECT LIST.
!
CALL SVESTB(0)
!
!     If the FFE is active:
!     The FFE CWD pools have not been loaded yet (FMMAIN & FMCBA have
!     yet to be called); therefore, temporarily load some fire model
!     tree attributes, then call FMCBA to load the initial CWD.
!     This will enable CWD objects to be displayed in the SVS
!     initial inventory display.
!
IF ( LFMON ) THEN
  DO I=1,ITRN
    FMPROB(I) = PROB(I)
    FMICR(I)  = ICR(I)
    FIRKIL(I) = 0.0
  ENDDO
  CALL FMCBA (IY(1),1)
ENDIF
IF ( DEBUG ) THEN
  WRITE(JOSTND,1020) ICYC
1020   FORMAT (' ','IN SVSTART, ICYC=',I2,':', / , &
             ' ',T5,'CWD ARRAY (UNPILED ONLY):')
  DO IH=1,2
    DO ID=1,4
      WRITE(JOSTND,1040) (CWD(1,IFC,IH,ID),IFC=1,11), IH, ID
1040       FORMAT(T5,11(F7.3,1X),'IH=',I1,', ID=',I1)
    ENDDO
  ENDDO
ENDIF
!
!     OUTPUT THE INITIAL PICTURE
!
CALL SVOUT(IY(1),0,'Inventory conditions')
!
!     If FFE variables were temporarily loaded for the initial SVS
!     display, reverse the process now, to avoid double-counting of
!     initial CWD loadings.
!
IF ( LFMON ) THEN
  DO I=1,ITRN
    FMPROB(I) = 0.0
    FMICR(I)  = 0
  ENDDO
  DO I = 1,3
    DO J = 1,MXFLCL
      DO K = 1,2
        DO L = 1,5
          CWD(I,J,K,L) = 0.0
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

!
!  Special processing for SO variant, when FFE is not active:
!
!  The SO variant is atypical in how the snagfall/decay coefficients
!  are loaded. For the SO variants, the snagfall/decay coefficients
!  are not loaded in FMVINIT. Their initialization is delayed until
!  FMCBA, because certain parameters are dependent on the forest
!  location code (whether you're in region 5 or 6), and the location
!  code either isn't set yet in FMVINIT, or, could change.
!  The following logic calls SOSNAG (an entry point in the SO/
!  FMCBA routine), to assign the missing snagfall/decay coefficients
!  when FFE is not active

IF ( .NOT. LFMON ) THEN
  IF (VARACD .EQ. 'SO' ) THEN
    CALL SNGCOE
  ENDIF
ENDIF

!
!     QUICK CHECKS/DEBUG
!
IF (DEBUG) CALL SVCDBH(WK3,0)
!
!
RETURN
END
