SUBROUTINE FMCADD
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!     CALLED FROM: FMMAIN
!     CALLS
!  Purpose:
!     This subroutine calculates annual litterfall and crown breakage
!     from each tree, and adds the material to the appropriate debris
!     pools.  It also adds in dead material from crown lifting and the
!     debris-in-waiting from previously killed snags (i.e. CWD2B).
!     CWD2B may include some dead material from
!     live-but-scorched trees. Crown foliage and woody materials are not
!     reduced for the losses calculated here: the material is assumed to
!     be replaced by growth (and is re-calculated to take account of
!     changes in tree size at the start of each cycle).  Data on leaf
!     lifespan come from Keane, Arno & Brown 1989.
!     NOTE:  PP leaf lifespan data was used for species 11 and WP
!     NOTE:  DF leaf lifespan data was used for species WH and Cedar
!----------------------------------------------------------------------
!
!  Local variable definitions:
!     DKCL:    decay class
!     DOWN:    pounds per acre of snag material falling in this year.
!     PDOWN:   the Proportion of material that has come DOWN in each
!              year class of CWD2B
!     MAXYR:   the number of year classes of CWD2B from which material
!              must be removed.
!
!  Common block variables and parameters:
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.

INCLUDE 'FMCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'

!.... Variable declarations.

  INTEGER I, SIZE, SP, DKCL, IYR
  REAL    DOWN, PDOWN
LOGICAL DEBUG
  REAL    AMT
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMCADD',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC,NYRS,TFMAX
7 FORMAT(' ENTERING FMCADD CYCLE = ',I2,' NYRS=',I4,' TFMAX=',I4)

!.... Begin routine.

!     Go through the tree list, calculating debris inputs from each tree.

DO I=1,ITRN

   IF (FMPROB(I).GT.0.) THEN

      SP = ISP(I)
      DKCL = DKRCLS(SP)

!           Litterfall from foliage:
      CWD(1,10,2,DKCL) = CWD(1,10,2,DKCL) &
           + (CROWNW(I,0) * FMPROB(I)/LEAFLF(SP)) * P2T
      CWDNEW(1,10) = CWDNEW(1,10) &
           + (CROWNW(I,0) * FMPROB(I)/LEAFLF(SP)) * P2T

      DO SIZE=1,5

!           Random breakage of each woody crown component:
          AMT = (LIMBRK * FMPROB(I) * CROWNW(I,SIZE)) &
                                       * P2T
          CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) + AMT
          CWDNEW(1,SIZE) = CWDNEW(1,SIZE) + AMT

!           Dead material from crown lifting (NOTE:  foliage isn't being
!           included here because it's assumed that the leaf-lifespan
!           data used above already incorporates the death of some foliage
!           due to crown lifting).
!           Additional test to avoid super small values that cause
!           floating point error. If record will contribute less
!           than 0.001 ounce per acre, AMT contribution is zero.
!           LRD 9/19/24

          IF ((FMPROB(I) * OLDCRW(I,SIZE)) .LT. 0.0000625) THEN
            AMT = 0.0
          ELSE
            AMT = (FMPROB(I) * OLDCRW(I,SIZE)) * P2T
          ENDIF

          CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) + AMT
          CWDNEW(1,SIZE) = CWDNEW(1,SIZE) + AMT

      ENDDO
   ENDIF
ENDDO


!     Add in the debris-in-waiting from snags.  Since crowns left during
!     salvage are handled elsewhere, it is enough to take
!     all the material in the year-1 pool of CWD2B.

    IYR=1
    PDOWN = 1.0

!       Repeat for each decay class:

  DO DKCL=1,4

!          First add the litterfall to down debris.

      DOWN = PDOWN * CWD2B(DKCL,0,IYR) / NYRS
     CWD(1,10,2,DKCL) = CWD(1,10,2,DKCL) + DOWN / 2000.0
     CWDNEW(1,10) = CWDNEW(1,10) + DOWN / 2000.0
     CWD2B(DKCL,0,IYR) = CWD2B(DKCL,0,IYR) - DOWN

!          Then all the sizes of woody material.

     DO SIZE=1,5
          DOWN = PDOWN * CWD2B(DKCL,SIZE,IYR) / NYRS
        CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) + DOWN / 2000.0
        CWDNEW(1,SIZE) = CWDNEW(1,SIZE) + DOWN / 2000.0
        CWD2B(DKCL,SIZE,IYR) = CWD2B(DKCL,SIZE,IYR) - DOWN

     ENDDO
  ENDDO

!     Write out debug values before pools are moved forward for the year.

  IF (DEBUG) THEN
    WRITE(JOSTND,*) 'BEFORE POOLS ARE MOVED FORWARD'
    DO DKCL=1,4
      DO SIZE=0,5
          DO IYR=1,(TFMAX-1)
             WRITE (JOSTND,10)DKCL,SIZE,IYR, &
                       CWD2B(DKCL,SIZE,IYR),CWD2B2(DKCL,SIZE,IYR)
ENDDO
      ENDDO
    ENDDO
  ENDIF

!     Move all the debris-in-waiting pools of each decay class forward
!     one year, to be ready for next year:

DO DKCL=1,4
   DO SIZE=0,5
      DO IYR=1,(TFMAX-1)
         CWD2B(DKCL,SIZE,IYR) = CWD2B(DKCL,SIZE,IYR+1)
      ENDDO
      CWD2B(DKCL,SIZE,TFMAX) = 0.0
   ENDDO
ENDDO

!     Write out debug values after pools are moved forward for the year.

IF (DEBUG) THEN
    WRITE(JOSTND,*) 'AFTER POOLS ARE MOVED FORWARD'
   DO DKCL=1,4
      DO SIZE=0,5
         DO IYR=1,(TFMAX-1)
            WRITE (JOSTND,10)DKCL,SIZE,IYR, &
                    CWD2B(DKCL,SIZE,IYR),CWD2B2(DKCL,SIZE,IYR)
10             FORMAT(' FMCADD: DKCL,SIZE,IYR=',3I3,' CWD2B=',F12.2, &
                    ' CWD2B2=',F12.2)
         ENDDO
      ENDDO
   ENDDO
ENDIF

RETURN
  END
