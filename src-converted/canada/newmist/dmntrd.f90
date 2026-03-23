SUBROUTINE DMNTRD
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMNTRD -- NISI  Date of last revision: April 10 1994
!----------------------------------------------------------------------
! Purpose:
!  Height and crown may change as each tree record is projected. In
! contrast, DM infections are fixed on the branches and do not move
! within the crown. This means that infection loads must be modified
! so that they accurately represent the relationship between
! nonmoving infections and moving crown third breakponts. In
! particular, new and uninfected crown is produced at the top, and
! crown death may occur at the lower crown margin. By locating the
! change in breakpoints, the proper proportions of existing infection
! density can be properly reassigned.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     DMRANN
!
! Argument list definitions:
!
!     [none]
!
! Local variable definitions:
!
!     INTEGER   i         Loop counter for crown thirds.
!     INTEGER   j         Loop counter for crown thirds.
!     INTEGER   k         Crown third in which breakpoint lies
!                          following growth.
!     INTEGER   u         Loop counter for treelist records.
!     INTEGER   v         Loop counter for life history pools
!     REAL      x         Sum of previous breakpoint values.
!     REAL      Wt        Scalar to assign correct proportion
!                          of infection in old crown third across
!                          breakpoint now lying in that crown third.
!     REAL      Mult      Scalar to adjust infection density as a
!                          function of the change in crown height
!                          over the growth period. See additional
!                          note in code below.
!     REAL      OldVal    Array of infection loads before growth, for
!                          and individual record. Array is
!                          dimensioned (1:CRTHRD) for each crown
!                          third, and (1:ACTIVE) for each life history
!                          pool.
!     REAL      NewVal     Array of infection loads after growth.
!                          Array is dimensioned like 'OldVal()'.
!
! Common block variables and parameters:
!
!     CRTHRD    DMCOM
!     ACTIVE    DMCOM
!     ITRN      CONTRL
!     BPCNT     DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'DMCOM.f90'

INTEGER   i, j, k, L,u, v
REAL      x, Wt, Mult, TINY
REAL      OldVal(CRTHRD,DEAD_BC)
REAL      NewVal(CRTHRD,DEAD_BC)
REAL      OldVal_BC(CRTHRD,ACTIVE,MAXBC)
REAL      NewVal_BC(CRTHRD,ACTIVE,MAXBC)

!     Protect against arithmetic underflow (gfortran crash)
!     during multiplication of small numbers.

DATA TINY / 1.0E-25 /

! LOOP OVER ALL TREES, REGARDLESS OF SPECIES OR INFECTION STATUS

DO 100 u = 1,ITRN

! WHEN THE TREELIST IS COMPRESSED, PREVIOUS BREAKPOINTS ARE SET TO
! ZERO. IF THE PREVIOUS BREAKPOINT ARRAY IS ALL ZERO, REALLOCATION OF
! INFECTION IS MEANINGLESS, AND A JUMP TO THE BOTTOM OF THE U-LOOP IS
! REQUIRED TO PREVENT A DIVIDE-BY-ZERO ERROR THAT WOULD OCCUR.

  x = 0.0
  DO i = 1,CRTHRD
    x = x + PBrkPt(u,i)
  ENDDO

  IF (x .EQ. 0.0) GOTO 100

! 'MULT' IS A BIT OF A KLUDGE. ITS INTENT IS TO APPROXIMATE CROWN
! GROWTH, WHICH ENLARGES (MOST) CROWNS, BUT DOES NOT INCREASE THE
! NUMBER OF INFECTIONS. SINCE 'DMINF()' HOLDS A MEASURE OF INFECTION
! DENSITY, IT IS NECESSARY TO SCALE THIS DENSITY DOWN WHEN THE CROWN
! ENLARGES, SO THAT THE ABSOLUTE AMOUNT OF INFECTION WILL REMAIN
! CONSTANT. IT IS A KLUDGE BECAUSE TO DO IT MORE ACCURATELY WOULD
! REQUIRE KEEPING TRACK OF CROWN GEOMETRY DURING TWO TIME PERIODS,
! WHICH WOULD REQUIRE A *LOT* OF MEMORY.

    Mult = (PBrkPt(u,1) - PBrkPt(u,BPCNT)) / &
               (BrkPnt(u,1) - BrkPnt(u,BPCNT))

    DO i = 1,CRTHRD
      DO v = 1,DEAD_BC
        OldVal(i,v) = DMINF(u,i,v) * Mult
        NewVal(i,v) = 0.0
      ENDDO
      DO v = 1,ACTIVE
        DO L = 1,MAXBC
          OldVal_BC(i,v,L) = DMINF_BC(u,i,v,L) * Mult
          NewVal_BC(i,v,L) = 0.0
        ENDDO
      ENDDO
    ENDDO

! FIND NEW CROWN THIRD 'J' IN WHICH EACH OLD BREAKPOINT 'I' LIES. A
! PREVIOUS CROWNTHIRD BREAKPOINT NO LONGER FOUND IN THE CROWN
! PRODUCES A (K .EQ. 0) CONDITION.

    DO i = 1,CRTHRD
      k = 0
      DO j = 1,CRTHRD
        IF((PBrkPt(u,i) .LE. BrkPnt(u,j)) &
             .AND. (PBrkPt(u,i) .GT. BrkPnt(u,j+1))) THEN
          k = j
          GOTO 450
        END IF
      ENDDO
450       CONTINUE

! IF ANY CROWN PORTIONS OVERLAP, TRANSFER INFECTION LEVEL. IN THE
! CASE (K .EQ. 0), THE ENTIRE CROWN THIRD HAS OUTGROWN IS PRIOR
! POSITION.

      IF (k .GT. 0) THEN
        IF (PBrkPt(u,i+1) .GE. BrkPnt(u,k+1)) THEN
          DO v = 1,DEAD_BC
            NewVal(k,v) = NewVal(k,v) + OldVal(i,v)
          ENDDO
          DO v = 1,ACTIVE
            DO L = 1,MAXBC
              NewVal_BC(k,v,L) = NewVal_BC(k,v,L) &
                    + OldVal_BC(i,v,L)
            ENDDO
          ENDDO
        ELSE
          Wt = (PBrkPt(u,i) - BrkPnt(u,k+1)) / &
                    (PBrkPt(u,i) - PBrkPt(u,i+1))
          DO v = 1,DEAD_BC
            IF (OldVal(i,v) .GT. TINY) &
                 NewVal(k,v) = NewVal(k,v) &
                  + (OldVal(i,v) * Wt)
          ENDDO
          DO v = 1,ACTIVE
            DO L = 1,MAXBC
              IF (OldVal_BC(i,v,L) .GT. TINY) &
                   NewVal_BC(k,v,L) = NewVal_BC(k,v,L) &
                    + (OldVal_BC(i,v,L) * Wt)
            ENDDO
          ENDDO
          IF (k .LT. CRTHRD) THEN
            DO v = 1,DEAD_BC
              IF (OldVal(i,v) .GT. TINY) &
                   NewVal(k+1,v) = NewVal(k+1,v) &
                   + (OldVal(i,v) * (1.0 - Wt))
            ENDDO
            DO v = 1,ACTIVE
              DO L = 1,MAXBC
                IF (OldVal_BC(i,v,L) .GT. TINY) &
                     NewVal_BC(k+1,v,L) = NewVal_BC(k+1,v,L) &
                     + (OldVal_BC(i,v,L) * (1.0 - Wt))
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDDO

! PLACE VALUES BACK IN CROWN THIRD CATEGORIES.

  DO i = 1,CRTHRD
    DO v = 1,DEAD_BC
      DMINF(u,i,v) = NewVal(i,v)
    ENDDO
    DO v = 1,ACTIVE
      DO L = 1,MAXBC
        DMINF_BC(u,i,v,L) = NewVal_BC(i,v,L)
      ENDDO
    ENDDO
  ENDDO

100 CONTINUE

RETURN
END
