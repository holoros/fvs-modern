SUBROUTINE RDDAM (II,ICODES)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This routine processes the root disease damage codes.
!
!  Called By :
!     DAMCDS  [FVS]
!
!  Calls :
!     DBCHK   (SUBROUTINE)   [PROGNOSIS]
!     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
!     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
!     RDSTP   (SUBROUTINE)   [ROOT DISEASE]
!
!  Arguments :
!     II     - INT, (I)
!              Tree pointer (tree record number).
!     ICODES - INT, (I)
!              Damage/Severity codes array.
!
!  FVS common tree level arrays used:
!     IMC    - Management Code replaced use of input Tree history code.
!     IDTREE - Tree identification.
!     PROB   - Tree tally ..not the sampling probability.
!     HT     - Recorded tree height.
!     DBH    - Recorded diameter.
!     ISP    - Tree species numberic code.
!     ITRE   - Plot identification.
!     ITRUNC - Recorded truncated height.
!
!  Local Variables :
!     IDAMC  - INT
!              Array which holds the damage codes for each root disease type.
!     NSRD   - INT
!              index to IDAMC array holding non-specific root disease damage code.
!
!  Common block variables used :
!
!  Revision History :
!  04/18/97 - Matthew K. Thompson
!    If a tree record's tree species was not of a root disease
!    host the model was trying to place it in a plot as an
!    uninfected tree.  Now if it is of a non-host species the tree
!    record is skipped and the subroutine is exited.
!  04/15/04 - Lance R. David (FHTET)
!    Added recognition of damage code 60 (non-specific root disease)
!    so that tree records with code 60 will be included with any
!    disease type (RRTYPE) specified for the simulation.
!  03/25/05 - Lance R. David (FHTeT)
!    Parenthesis were added to the conditional statements in the
!    processing of the damage codes. The default order of evaluation
!    resulted in (A or (B and C and D)) in stead of the correct
!    evaluation order of ((A or B) and C and D). This resulted in
!    infected trees being interpreted as uninfected.
!  07/10/07 - Lance R. David (FHTET)
!    Time at which damage codes are processed is now at the end of
!    keyword processing instead of during the reading of tree data.
!    So, tree data items that were passed as arguments are now
!    available from the FVS common area. Original arguments were:
!    (II,IITH,IDD,ICODES,PROBB,HHT,DDBH,IISPI,ITREII,TTHT)
!    FVS array IMC(II) is used as replacement for input tree history
!    code (IITH).
!    No need for special handling of dead tree index (IREC2) because
!    dead trees are already at the end of the arrays. Argument II is
!    correct index value for both live and dead.
!   08/28/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!----------
!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'HTCAL.f90'
INCLUDE 'ESTREE.f90'
INCLUDE 'RDADD.f90'

!.... Argument variable declarations.

INTEGER ICODES(6), II

!.... Local variable declarations.

INTEGER IDAMC(ITOTRR+1), J, K, NSRD
INTEGER IFIADAM(ITOTRR+1)
LOGICAL DEBUG

!.... Data statements.
!.... Input tree data root disease damage codes
!.... RD RRTYPE        FVS CODE  FIA CODE
!....   1-Annos-P         64     21010
!....   2-Annos-S         64     21010
!....   3-Armillaria      61     21001
!....   4-Phellinus       62     21017
!....   5-non-specified   60     21000
!....     root disease
!.... RRTYPE CODE 1  2  3  4
DATA IDAMC /64,64,61,62,60/
DATA IFIADAM /21010, 21010, 21001, 21017, 21000/
DATA NSRD /5/

!.... See if we need to do some debug.

CALL DBCHK (DEBUG,'RDDAM',5,ICYC)
IF (DEBUG) &
     WRITE (JOSTND,10) II,IMC(II),IDTREE(II),ICODES,PROB(II), &
                       HT(II),DBH(II),ISP(II),ITRE(II),ITRUNC(II)
10 FORMAT(' ** IN RDDAM: II=', I4,' IMC=',I1,' IDTREE=',I4, &
          ' ICODES=',6(I3),' PROB=',F8.3,' HT=',F6.2,' DBH=',F5.2, &
          ' ISP=',I2,' PLOT=',I3,' THT=',I8)

!.... If infected plots are not specified then branch to statement 200.
!.... LONECT is initialized in RDBLK1 to 0 and when the PLREAD keyword
!.... is used it is set to 2 (multiple plots).

IRRSP = MAXRR
IF (MAXRR .LT. 3) IRRSP = IDITYP(IRTSPC(ISP(II)))

IF (DEBUG) WRITE (JOSTND,*) &
    '              IRRSP=',IRRSP,' IDAMC=',IDAMC

!.... If the tree record is of a non-host species then exit.

IF (IRRSP .EQ. 0) GOTO 9000

!.... Set the plot id for use if PLOTINF keyword is used.
!.... If trees are outside all plots of interest (and therefore outside centers)
!.... then a plot id is not assigned (IDPLOT = 0).

IF (LPLINF) THEN
   IDPLOT(II) = 0

   DO 90 K=1,50
      IF (ITRE(II) .EQ. IANPLT(IRRSP,K)) THEN
         IDPLOT(II) = ITRE(II)
         GOTO 95
      ENDIF
90    CONTINUE

95    CONTINUE
ENDIF

!.... If PLREAD or PLOTINF are not being used then don't test the tree
!.... record against plot information.

IF (LONECT(IRRSP) .NE. 2) GOTO 200

!.... See if the tree is in a diseased sub-plot.

DO 100 K=1,50
   IF (ITRE(II) .EQ. IRDPLT(IRRSP,K)) GOTO 200
100 CONTINUE
GOTO 9000

200 CONTINUE

!.... Process tree record.  Check to see if tree record represents a
!.... dead tree.
!....
!.... (Note : When we allow more than one root disease species, more
!.... than one damage code may be allowed to be read on a tree record.

IF (IMC(II) .EQ. 7 .OR. IMC(II) .EQ. 9) THEN

!....    Check to see if dead tree is a root diseased stump.
!....    It is a diseased stump if it has a root disease damage code or
!....    it has a height of 1.5 feet or less (0 < HT(II) <= 1.5).
!....    Trees that have been cut in previous rotations and have now
!....    been infected with root disease are flagged as stumps by
!....    recording them as dead and recording a value of 1.5 in the
!....    variable HT(II).
!....
!....    FVS CODES CHECKED FIRST AND THEN FIA CODES
   IF (ICODES(1) .EQ. IDAMC(IRRSP) .OR. &
          ICODES(1) .EQ. IDAMC(NSRD) .OR. &
          ICODES(3) .EQ. IDAMC(IRRSP) .OR. &
          ICODES(3) .EQ. IDAMC(NSRD) .OR. &
          ICODES(5) .EQ. IDAMC(IRRSP) .OR. &
          ICODES(5) .EQ. IDAMC(NSRD) .OR. &
          ICODES(1) .EQ. IFIADAM(IRRSP) .OR. &
          ICODES(1) .EQ. IFIADAM(NSRD) .OR. &
          ICODES(3) .EQ. IFIADAM(IRRSP) .OR. &
          ICODES(3) .EQ. IFIADAM(NSRD) .OR. &
          ICODES(5) .EQ. IFIADAM(IRRSP) .OR. &
          ICODES(5) .EQ. IFIADAM(NSRD) .OR. &
          (HT(II) .GT. 0 .AND. HT(II) .LE. 1.5)) THEN

!....       Place a flag in the tree list that identifies this as an infected
!....       dead tree. The final stump calculations will now be done in RDPRIN
!....       once the density of stumps is calculated.

      IPRFL(II) = 4

      RISTU(IRRSP) = RISTU(IRRSP) + 1

      IF (DEBUG) WRITE (JOSTND,1350) IDTREE(II)
1350       FORMAT (' RECORD NUMBER: ',I7,' IS AN INFECTED STUMP')
   ENDIF

ELSE

!....    Process live trees to determine if infected or uninfected.
!....    'Suspect' trees are considered to be uninfected. These are
!....    trees that are within 30 feet of a tree killed by root disease
!....    and whose DBH < 5".  This recommendation for 'suspect' trees was
!....    made by Byler and Goheen.

   DO 900 J=1,5,2
      IF((ICODES(J) .EQ. IDAMC(IRRSP) .OR. &
             ICODES(J) .EQ. IDAMC(NSRD)  .OR. &
             ICODES(J) .EQ. IFIADAM(IRRSP) .OR. &
             ICODES(J) .EQ. IFIADAM(NSRD)) &
            .AND. ICODES(J+1) .EQ. 1 &
            .AND. DBH(II) .LE. 5.0) THEN

!....          Tree is uninfected but within a center.

         IF (DEBUG) WRITE (JOSTND,1050) IDTREE(II), J
1050          FORMAT (' RECORD NUMBER: ',I7,' IS UNINFECTED A, J=',I1)
         IPRFL(II) = 5
         RINUF(IRRSP) = RINUF(IRRSP) + 1.0
         GOTO 9000

      ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR. &
                 ICODES(J) .EQ. IDAMC(NSRD)  .OR. &
                 ICODES(J) .EQ. IFIADAM(IRRSP) .OR. &
                 ICODES(J) .EQ. IFIADAM(NSRD)) &
                .AND. ICODES(J+1) .LE. 1 &
                .AND. DBH(II) .GT. 5.0) THEN

!....          Tree is within 30 feet of infected tree.

         IF (DEBUG) WRITE (JOSTND,1150) IDTREE(II), ICODES(J+1)
1150          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ', &
                    I5)
         IPRFL(II) = 1
         RINNF(IRRSP) = RINNF(IRRSP) + 1.0
         GOTO 9000

      ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR. &
                 ICODES(J) .EQ. IDAMC(NSRD)  .OR. &
                 ICODES(J) .EQ. IFIADAM(IRRSP) .OR. &
                 ICODES(J) .EQ. IFIADAM(NSRD)) &
                .AND. ICODES(J+1) .EQ. 2) THEN

!....          Pathogen or diagnostic symptoms detected.

         IF (DEBUG) WRITE (JOSTND,1250) IDTREE(II), ICODES(J+1)
1250          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ', &
                    I5)
         IPRFL(II) = 2
         RINNF(IRRSP) = RINNF(IRRSP) + 1.0
         GOTO 9000

      ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR. &
                 ICODES(J) .EQ. IDAMC(NSRD)  .OR. &
                 ICODES(J) .EQ. IFIADAM(IRRSP) .OR. &
                 ICODES(J) .EQ. IFIADAM(NSRD)) &
                .AND. ICODES(J+1) .EQ. 3) THEN

!....          Crown deterioration.

         IF (DEBUG) WRITE (JOSTND,1750) IDTREE(II), ICODES(J+1)
1750          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ', &
                    I5)
         IPRFL(II) = 3
         RINNF(IRRSP) = RINNF(IRRSP) + 1.0
         GOTO 9000

      ENDIF

900    CONTINUE

   IF (PAREA(IRRSP) .GT. 0.0) THEN

!....       Tree is uninfected but within a center.

      IF (DEBUG) WRITE (JOSTND,1850) IDTREE(II), J
1850       FORMAT (' RECORD NUMBER: ',I7,' IS UNINFECTED B, J=',I1)
      IPRFL(II) = 5
      RINUF(IRRSP) = RINUF(IRRSP) + 1.0
      GOTO 9000
   ENDIF

ENDIF

9000 CONTINUE
RETURN
END
