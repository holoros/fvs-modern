SUBROUTINE RDBBDO
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Determines for each component of each tree record the maximum
!     mortality that any one of the bark beetles that are currently
!     eligible to act could inflict on that tree record, and the number
!     of trees killed by applying this mortality rate.  Reports the
!     total number of trees killed via :
!     OAKL(DSO/DSIU/DSII,#TREES), RRKILL(#TREES), and
!     RROBNK(#TREESPECIES).
!
!      **************************************************************
!      ** DOES NOT RECALCULATE THE NUMBER OF LIVE TREES REMAINING, **
!      ** except in the case of PROBI(#TREES,#GROWTHCYCLES) and    **
!      ** PROBIT(#TREES).                                          **
!      **************************************************************
!
!  Calls:
!     RDSUM   [ROOT DISEASE]
!
!  Called By :
!     RDOAGM  [ROOT DISEASE]
!
!  Local Variables :
!     DSF    - INTEGER
!              Dead Standing Fringe (only calculated when BB4 is active
!              (will DSF=0 if BB4 is not active, because FRINGE=0 unless
!              BB4 is active)) (note windthrow is included in all cases,
!              "standing" dead is not really accurate)
!     MAXKL  - REAL
!              The maximum mortality rate applicable to the current
!              component of the current tree record (other than in
!              'fringe' areas).
!     MAXFKL - REAL
!              The maximum fringe-specific mortality rate applicable
!              to the current component of the current tree record.
!     NUMDED - REAL
!              The number of trees in the current component of the
!              current tree record that just died from bark beetle
!              attacks.
!     NUMDEF - REAL
!              NUMDED for trees in the "fringe".
!
!  Revision History :
!    06/12/96 - Matthew K. Thompson
!      Moved the declaration of DSO, DSII, and DSIU to the
!      parameter include file RDPARM.
!    12-JUL-2002 Lance R. David
!      Added debug code.
!   08/26/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!----------------------

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations.

INTEGER DSF, RRTYPE, I, IK, BB, ISPI, IP, ISL
REAL    MAXKL, MAXFKL, NUMDED, NUMDEF, TOTDED
LOGICAL DEBUG

!.... Data statements.

DATA DSF /4/

!.... SEE IF WE NEED TO DO SOME DEBUG.

CALL DBCHK (DEBUG,'RDBBDO',6,ICYC)
IF (DEBUG) WRITE (JOSTND,*) 'ENTER: RDBBDO'

IF (ITRN .LE. 0) RETURN

!.... Look at every tree record.  Figure out what tree species it is
!.... and what root rot type attacks it.

DO 999 I = 1,ITRN
   ISPI = ISP(I)
   RRTYPE = MAXRR
   IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))

   IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO I=',I,' ISPI=',ISPI, &
      ' RRTYPE=',RRTYPE

!....    Kill Inside Infected trees:  look at the portion of the record
!....    infected in each time period, find the maximum
!....    bark-beetle-induced mortality that applies to it, and kill it
!....    accordingly.

   TOTDED = 0.0

   DO 333 IK = 1,ISTEP
      DO 255 IP = 1,2
         IF (PROBI(I,IK,IP) .EQ. 0.0) GOTO 255
         MAXKL = 0.0

         DO 222 BB = 1,NUMBB
            IF ((HOST(BB).EQ.ISPI) .AND. &
                   (MINDBH(BB).LE.DBH(I))) THEN

               IF (MININF(BB) .LE. PROPI(I,IK,IP)) THEN
                  IF (IIRATE(BB) .GT. MAXKL) MAXKL = IIRATE(BB)
               ELSE
                  IF (IURATE(BB) .GT. MAXKL) MAXKL = IURATE(BB)
               ENDIF

            ENDIF
222          CONTINUE

         IF (MAXKL .GT. 0.0) THEN
            NUMDED = PROBI(I,IK,IP) * MAXKL
            PROBI(I,IK,IP) = PROBI(I,IK,IP) - NUMDED
            OAKL(DSII,I) = OAKL(DSII,I) + NUMDED
            BBKILL(DSII,I) = BBKILL(DSII,I) + NUMDED
            RRKILL(I) = RRKILL(I) + NUMDED
            RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
            TOTDED = TOTDED + NUMDED
         ENDIF
255       CONTINUE
333    CONTINUE

!....    Update the stump list with beetle-killed, infected trees.

   IF (TOTDED .GT. 0.0) THEN
      CALL RDSSIZ(ISPI,DBH(I),STCUT,ISL,ISPS,IRTSPC)
      CALL RDSTP (ISL,ISPI,TOTDED,DBH(I),ROOTL(I))
   ENDIF

!....    Kill Inside Uninfected trees:  find the maximum applicable
!....    mortality, and apply it.

   IF (PROBIU(I) .EQ. 0.0) GOTO 555
   MAXKL = 0.0

   DO 444 BB = 1,NUMBB
      IF ((HOST(BB) .EQ. ISPI) &
             .AND. (MINDBH(BB) .LE. DBH(I)) &
             .AND. (IURATE(BB) .GT. MAXKL)) THEN
         MAXKL = IURATE(BB)
      ENDIF
444    CONTINUE

   IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO PROBIU=',PROBIU(I), &
      ' MAXKL=',MAXKL,' KILL INSIDE UNINFECTED'

   IF (MAXKL .GT. 0.0) THEN
      NUMDED = PROBIU(I) * MAXKL
      OAKL(DSIU,I) = OAKL(DSIU,I) + NUMDED
      BBKILL(DSIU,I) = BBKILL(DSIU,I) + NUMDED
      RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
      IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO NUMDED=',NUMDED, &
         ' DSIU=',DSIU,' OAKL=',OAKL(DSIU,I),' BBKILL=', &
         BBKILL(DSIU,I),' RROBNK=',RROBNK(ISPI)
   ENDIF

555    CONTINUE

!....    Kill Outside trees:  find the maximum mortality applicable
!....    to trees in the "fringe" and non-fringe areas of the stand,
!....    and apply these mortality rates in proportion to the number
!....    of trees in each area.   Note that FPROB is a density of trees,
!....    not a total number of trees.

   IF (FPROB(I) .EQ. 0.0) GOTO 777
   MAXKL = 0.0
   MAXFKL = 0.0

   DO 666 BB = 1, NUMBB
      IF ((HOST(BB) .EQ. ISPI) .AND. &
             (MINDBH(BB) .LE. DBH(I))) THEN
         IF (ORATE(BB) .GT. MAXKL) MAXKL = ORATE(BB)
         IF (OFRATE(BB) .GT. MAXFKL) MAXFKL = OFRATE(BB)
      ENDIF
666    CONTINUE

   IF ((MAXKL .GT. 0.0) .OR. (MAXFKL .GT. 0.0)) THEN
      NUMDEF = FPROB(I) * FRINGE(RRTYPE) * MAXFKL
      NUMDED = FPROB(I) *(SAREA-PAREA(RRTYPE)-FRINGE(RRTYPE)) * &
                  MAXKL + NUMDEF
      BBKILL(DSF,I) = BBKILL(DSF,I) + NUMDEF
      BBKILL(DSO,I) = BBKILL(DSO,I) + NUMDED
      OAKL(DSO,I) = OAKL(DSO,I) + NUMDED
      RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
      FFPROB(I,2) = FFPROB(I,2) - (FFPROB(I,2) * MAXFKL)
   ENDIF

777    CONTINUE

!....    End of loop over tree records.

999 CONTINUE

!.... Get PROBIT totalled up for the new PROBI values.

CALL RDSUM (ITRN,PROBIT,PROBI,ISTEP)

IF (DEBUG) WRITE (JOSTND,*) 'EXIT: RDBBDO'

RETURN
END
