SUBROUTINE BMCSPT(ISTD,ISIZ,IPASS,IYR)
!----------
! WWPB $Id$
!----------
!     CALLED FROM: BMCNUM
!**********************************************************************
!  **BMCSPT  Date of last revision:  March 28, 1994
!----------------------------------------------------------------------
!  Purpose:
!----------------------------------------------------------------------
!
!  Local variable definitions:
!     II:     Loop counter over max # special tree types
!     IJ:     Loop counter over
!     ISIZ:   Loop counter over dbh size classes
!     SP:     Holder for prop'n special trees of each type
!
!  Common block variables and parameters:
!     ATRPHE: PROPORTION of trees with attractant pheromone
!     OTHATT: PROPORTION of trees attacked, but not killed
!     PBSPEC: Pine Beetle specie(s) being simulated
!     PITCH:  PROPORTION of trees with pitch-out or strip-kill
!     SCORCH: PROPORTION of trees in size class severely scorched by fire
!     SPCLT:  PROPORTION of special trees in each dbh class
!     STRIKE: PROPORTION of trees in class struck by lightning
!     TOPKLL: PROPORTION of stems attacked by Ips in each size class
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!.... Common include files.

INCLUDE 'BMCOM.f90'

!.... Variable declarations.

LOGICAL TOPFLAG, BOTFLAG
INTEGER II, IJ, IK, JJ, JK
INTEGER SPTCNT
REAL    SP(MXSPTT)
REAL    XSPLT
REAL    MT, MTT, MTTT


IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD, ISIZ, IPASS
10 FORMAT(' Begin BMCSPT: Year= ',I5, 'Stand= ', I6, &
           'Size class= ', I3, 'Pass= ', I2)


!.... if there are no trees in size class then don't need to calc anything

XSPLT= 0.0
IF (TREE(ISTD,ISIZ,1) .LE. 1.0E-9) GOTO 9999

!....   calculate prop. special trees

!....   Mountain Pine Beetle

IF ((PBSPEC .EQ. 1) .AND. (IPASS .EQ. 1)) THEN

  SP(1)= PITCH(ISTD,ISIZ)
  SP(2)= STRIKE(ISTD,ISIZ)
  SP(3)= TOPKLL(ISTD,ISIZ) + OTHATT(ISTD,ISIZ)
  SP(4)= SCORCH(ISTD,ISIZ)
  SP(5)= ATRPHE(ISTD)
!        SP(6)= RROT(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
!        IF (SP(6) .GT. 1.0) SP(6) = 1.0

  SPTCNT= 5

!....      Western Pine Beetle

ELSEIF ((PBSPEC .EQ. 2) .AND. (IPASS .EQ. 1)) THEN

  SP(1)= PITCH(ISTD,ISIZ)
  SP(2)= STRIKE(ISTD,ISIZ)
  SP(3)= TOPKLL(ISTD,ISIZ) + OTHATT(ISTD,ISIZ)
  SP(4)= SCORCH(ISTD,ISIZ)
  SP(5)= ATRPHE(ISTD)
!        SP(6)= RROT(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
!        IF (SP(6) .GT. 1.0) SP(6) = 1.0

  SPTCNT= 5

ELSEIF ((PBSPEC .EQ. 3) .OR. (IPASS .EQ. 2)) THEN

!....      Ips
!          The other things Ips like are not living trees
!          so they are used in calculations elsewhere
!
  SP(1)= OTHATT(ISTD,ISIZ)
  SP(2)= ATRPHE(ISTD)
  SPTCNT= 2

ENDIF

TOPFLAG= .TRUE.
DO 20 I= 1,SPTCNT
  IF (SP(I) .EQ. 1.0) THEN
    TOPFLAG= .FALSE.
    GOTO 21
  ENDIF
20 CONTINUE
21 CONTINUE

BOTFLAG= .FALSE.
DO 30 I= 1,SPTCNT
  IF (SP(I) .NE. 0.0) THEN
    BOTFLAG= .TRUE.
    GOTO 31
  ENDIF
30 CONTINUE
31 CONTINUE

IF (TOPFLAG .AND. BOTFLAG) THEN

  DO 1000 II= 1,SPTCNT

!....          special trees if no overlap

    XSPLT= XSPLT + SP(II)

    DO 1100 IJ= II+1,SPTCNT

!....             all this multiplication accounts for
!                 overlap. eg. struck trees that were
!                 pitchouts and fire scorched etc.

      MT= SP(II) * SP(IJ)
      DO 1110 IK= IJ + 1,SPTCNT
        MTT= MT * SP(IK)
        DO 1112 JJ= IK + 1,SPTCNT
          MTTT= MTT * SP(JJ)
          DO 1115 JK= JK + 1,SPTCNT
            XSPLT= XSPLT - MT - MTT - MTTT - (MTTT * SP(JK))
1115           CONTINUE
1112         CONTINUE
1110       CONTINUE
1100     CONTINUE
1000   CONTINUE

  IF (XSPLT .GT. 1.0) XSPLT= 1.0

ELSEIF (.NOT.BOTFLAG) THEN

!....       if all props= 0 then special= 0

      XSPLT= 0.0

ELSEIF (.NOT.TOPFLAG) THEN

!....    if any one of the props= 1 then special= 1

   XSPLT= 1.0

ENDIF


9999 CONTINUE

SPCLT(ISTD,ISIZ,IPASS)= XSPLT

IF(LBMDEB) WRITE(JBMBPR,11) IYR, ISTD
11 FORMAT(' End BMCSPT: Year= ',I5, 'Stand= ', I6)

RETURN
END
