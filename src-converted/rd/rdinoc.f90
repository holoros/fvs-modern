SUBROUTINE RDINOC(LICALL)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Decomposes infected root systems in the dead tree/stump list.
!
!  Called By :
!     RDSETP  [ROOT DISEASE]
!     RDTREG  [ROOT DISEASE]
!
!  Calls :
!     RDSLP   (FUNCTION)   [ROOT DISEASE]
!
!  Common block variables used :
!     DECRAT - Decay rate of roots.
!     JRSIT  - Number of years to 'sit' without decreasing root
!              radius
!     ROTSIT - Root radius at which to begin 'sitting' without
!              decreasing (feet)
!
!  Local variables :
!
!     DEBUG  - Logical flag to turn debug on or off.
!     JINT   - Interval over which to decay roots.
!     LICALL - .TRUE. = Called during initialization, .FALSE. = Called
!              elsewhere.
!     RTODEC - Root radius to decay this cycle.
!     RTREM  - Root radius remaining after decay.
!
! Revision History:
!   26-JUN-2002 Lance David (FHTET)
!     (last revision date noted was 3/25/97)
!     Added debug code to track down devide by zero error that has
!     occurred.
!   30-JUL-2002 Lance David (FHTET)
!     Added check and reset of root radius when current radius is
!     less than the value calculated as the radius at which the
!     root system is suppose to sit for x years.
!   08/28/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
!
!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'RDADD.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'PLOT.f90'

!.... Local variables.

INTEGER  I, IDI, J, JINT, K
REAL     DROOTS, TMINLF, RTODEC, RTREM

LOGICAL  DEBUG, LICALL

!.... See if we need to do some debug.

CALL DBCHK (DEBUG,'RDINOC',6,ICYC)

IF (DEBUG) THEN
   WRITE (JOSTND,900) ICYC,LICALL
900    FORMAT (' Begin RDINOC, Cycle = ', I5,' LICALL=',L)
ENDIF

JINT = INT(FINT)

DO 600 IDI=MINRR,MAXRR
   DO 500 I=1, 2
      DO 400 J=1, 5
         DO 300 K=1, ISTEP
            IF (DEBUG) WRITE (JOSTND,*) &
               'IN RDINOC: IDI=',IDI,' I=',I,' J=',J,' K=',K

            IF (PROBDA(IDI,I,J,K) .NE. 0.0 .AND. &
                   DBHDA(IDI,I,J,K) .NE. 0.0) THEN

               IF (LICALL) THEN
                  JINT = ABS(JRAGED(IDI,I,J,K))
                  JRAGED(IDI,I,J,K) = 0
               ENDIF

               ROTSIT = RSITFN(IDI,1) * DBHDA(IDI,I,J,K) + &
                           RSITFN(IDI,2)

!....                If the current root radius ROOTDA stored for the class
!....                is less than that calculated as the radius at which
!....                the root systems sits ROTSIT for x years, set the
!....                ROOTDA to ROTSIT. So that the decay process does
!....                not try to grow the root system. The weighted averaging
!....                of individuals into the class may have caused this
!....                backward condition to occur. LRD 30JUL02

               IF (ROOTDA(IDI,I,J,K) .LT. ROTSIT) THEN
                  ROOTDA(IDI,I,J,K) = ROTSIT
               ENDIF

!....                Armillaria & Phellinus change functions at
!....                12in DBH.

               IF (DBHDA(IDI,I,J,K) .LE. 12.0) THEN
                  JRSIT = INT(YRSITF(IDI,1,1) * &
                             DBHDA(IDI,I,J,K) + &
                             YRSITF(IDI,2,1))
               ELSE
                  JRSIT = INT(YRSITF(IDI,1,2) * &
                             DBHDA(IDI,I,J,K) + &
                             YRSITF(IDI,2,2))
               ENDIF

               IF (DEBUG) WRITE (JOSTND,*) &
                  'IN RDINOC: DBHDA=',DBHDA(IDI,I,J,K),' JINT=',JINT, &
                  ' JRAGED=',JRAGED(IDI,I,J,K),' ROTSIT=',ROTSIT, &
                  ' JRSIT=',JRSIT,'PROBDA=',PROBDA(IDI,I,J,K), &
                  ' RSITFN1=',RSITFN(IDI,1),' RSITFN2=',RSITFN(IDI,2)

               IF (JRAGED(IDI,I,J,K) .LE. 0) THEN

                  IF (DECRAT(IDI,I,J,K) .LE. 0.0) THEN

!....                      If stump has not yet started decaying
!....                      then find the decay rate.  The decay
!....                      rate is a function of root radius only.

                     IF (DBHDA(IDI,I,J,K) .LE. 12.0) THEN
                        DECRAT(IDI,I,J,K) = (DECFN(IDI,1,1) * &
                                               ROOTDA(IDI,I,J,K) + &
                                               DECFN(IDI,2,1)) / &
                                               DSFAC(I)
                     ELSE
                        DECRAT(IDI,I,J,K) = (DECFN(IDI,1,2) * &
                                               ROOTDA(IDI,I,J,K) + &
                                               DECFN(IDI,2,2)) / &
                                               DSFAC(I)
                     ENDIF

!....                      Modify the decay rate to account for the
!....                      minimum lifespan of inoculum (default is 0
!....                      but user may change)

                     DROOTS = ROOTDA(IDI,I,J,K) - ROTSIT

                     TMINLF = JRSIT + DROOTS / DECRAT(IDI,I,J,K)

                     IF (DEBUG) WRITE (JOSTND,*) &
                        'IN RDINOC: DECRAT=',DECRAT(IDI,I,J,K), &
                        ' ROOTDA=',ROOTDA(IDI,I,J,K), &
                        ' DROOTS=',DROOTS,' TMINLF=',TMINLF, &
                        ' XMINLF=',XMINLF(IDI)

                     IF (TMINLF .LT. XMINLF(IDI)) THEN
                        DECRAT(IDI,I,J,K) = DROOTS / &
                                               (XMINLF(IDI) - JRSIT)
                     ENDIF
                  ENDIF

!....                   If stump decay has not yet reached the 'core'
!....                   part then keep decreasing the root radius.

                  RTODEC = DECRAT(IDI,I,J,K) * JINT
                  RTREM = ROOTDA(IDI,I,J,K) - RTODEC

                  IF (RTREM .LT. ROTSIT) THEN
                     RTODEC = ROOTDA(IDI,I,J,K) - ROTSIT
                     IF (RTODEC .LT. 0.0) RTODEC = 0.0
                     JRAGED(IDI,I,J,K) = JINT - INT(RTODEC / &
                                            DECRAT(IDI,I,J,K))
                     RTREM = ROTSIT
                  ENDIF

                  ROOTDA(IDI,I,J,K) = RTREM
               ELSE

                  JRAGED(IDI,I,J,K) = JRAGED(IDI,I,J,K) + JINT
               ENDIF

               IF (JRAGED(IDI,I,J,K) .GT. JRSIT) THEN

!....                   If stump has sat with no apparent decay for
!....                   enough time then it disappears.

                  PROBDA(IDI,I,J,K) = 0.0
                  DBHDA(IDI,I,J,K)  = 0.0
                  JRAGED(IDI,I,J,K) = 0
                  ROOTDA(IDI,I,J,K) = 0.0
               ENDIF
            ENDIF

300          CONTINUE
400       CONTINUE
500    CONTINUE
600 CONTINUE

IF (DEBUG) THEN
   WRITE (JOSTND,910) ICYC
ENDIF

910 FORMAT (' End RDINOC, Cycle = ', I5)

RETURN
END
