! =============================================================================
! dgf_r_wrapper.f90
!
! Thin Fortran wrappers exposing fvs-modern's stateful DGF and HTGF subroutines
! through a clean argument-passing interface that R can call via .Fortran().
!
! Why this file exists:
!   The fvs-modern subroutines DGF(DIAM) and HTGF read and write through
!   roughly ten COMMON blocks (PRGPRM, CALCOM, ARRAYS, COEFFS, CONTRL,
!   OUTCOM, PLOT, PDEN, VARCOM, MULTCM, HTCAL). Calling them from R with
!   .Fortran() requires populating those blocks first, which is impractical
!   to do from R side. These wrappers do the marshalling once in Fortran
!   so R can pass plain arrays of inputs and receive plain arrays of
!   outputs.
!
! Build (CONUS unified variant):
!   gfortran -fPIC -shared -O2 -I src-converted/base \
!       src-converted/base/*.f90 \
!       src-converted/cn/dgf.f90 \
!       src-converted/cn/htgf.f90 \
!       dgf_r_wrapper.f90 \
!       -o libfvsmodern_cn.so
!
! Load in R:
!   dyn.load("libfvsmodern_cn.so")
!   .fvsmodern_loaded <- TRUE
!   # Then dgf_call() and htgf_call() in fvsRemodeled_CONUS.R will dispatch
!   # to the Fortran path automatically.
!
! Author   Aaron Weiskittel
! License  Public domain (matches the upstream USFS FVS license)
! =============================================================================


!------------------------------------------------------------------------------
! DGF_R: vectorised Diameter Growth Function wrapper
!------------------------------------------------------------------------------
SUBROUTINE DGF_R(NTREES, ISPCD_IN, DBH_IN, HT_IN, CR_IN, BAL_IN, &
                 ELEV_IN, EMT_IN, NYRS, DBH_OUT, IERR)
  IMPLICIT NONE

  ! Argument declarations -- keep these REAL (single precision) to match
  ! fvs-modern's default precision; on the R side, .Fortran() callers must
  ! use as.single() to avoid silent precision corruption.
  INTEGER, INTENT(IN)  :: NTREES
  INTEGER, INTENT(IN)  :: ISPCD_IN(NTREES)
  REAL,    INTENT(IN)  :: DBH_IN(NTREES)
  REAL,    INTENT(IN)  :: HT_IN(NTREES)
  REAL,    INTENT(IN)  :: CR_IN(NTREES)
  REAL,    INTENT(IN)  :: BAL_IN(NTREES)
  REAL,    INTENT(IN)  :: ELEV_IN
  REAL,    INTENT(IN)  :: EMT_IN
  INTEGER, INTENT(IN)  :: NYRS
  REAL,    INTENT(OUT) :: DBH_OUT(NTREES)
  INTEGER, INTENT(OUT) :: IERR

  ! Pull in the COMMON blocks DGF reads/writes.
  INCLUDE 'PRGPRM.f90'
  INCLUDE 'CALCOM.f90'
  INCLUDE 'ARRAYS.f90'
  INCLUDE 'COEFFS.f90'
  INCLUDE 'CONTRL.f90'
  INCLUDE 'OUTCOM.f90'
  INCLUDE 'PLOT.f90'
  INCLUDE 'PDEN.f90'
  INCLUDE 'VARCOM.f90'

  REAL    :: DIAM(MAXTRE)
  INTEGER :: I, IY
  REAL    :: DDS_LN, DBH_NEW

  IERR = 0

  ! Defensive bounds: too many trees would smash the COMMON arrays.
  IF (NTREES > MAXTRE) THEN
     IERR = 1
     RETURN
  END IF

  ! Marshal scalar plot state.
  ELEV   = ELEV_IN
  ICYC   = 1
  ITRTYP = 1

  ! Per-tree state. ISP(I) is the variant species index (1..MAXSP);
  ! map ISPCD_IN (FIA SPCD) to ISP via the JTYPE array loaded from cn.json
  ! during program init. Here we assume SPCD_TO_INDEX has already been
  ! resolved by the caller -- if not, replace with a lookup against JTYPE.
  DO I = 1, NTREES
     ISP(I)   = SPCD_TO_INDEX(ISPCD_IN(I))   ! external helper, see notes
     DBH(I)   = DBH_IN(I)
     HT(I)    = HT_IN(I)
     ICR(I)   = INT(CR_IN(I) * 100.0)        ! ICR is integer percent
     PROB(I)  = 1.0
     PTBAA(I) = BAL_IN(I)
     XMAXPT(I) = 1.0                          ! relative density placeholder
     PCT(I)    = 0.0
     ITRE(I)   = I
     DIAM(I)   = DBH_IN(I)
  END DO

  ! Drive the original DGF for NYRS years. The NC variant DGF returns
  ! WK2(I) = LN(change in squared diameter) over the variant's standard
  ! cycle length (10 years for legacy, but FINT controls this in CONTRL).
  ! For an annual loop we set FINT=1 and call NYRS times.
  FINT = 1.0
  DO IY = 1, NYRS
     CALL DGF(DIAM)
     DO I = 1, NTREES
        DDS_LN  = WK2(I)
        DBH_NEW = SQRT(MAX(DIAM(I)**2 + EXP(DDS_LN), 0.0))
        DIAM(I) = DBH_NEW
        DBH(I)  = DBH_NEW
     END DO
  END DO

  DO I = 1, NTREES
     DBH_OUT(I) = DIAM(I)
  END DO

  RETURN

CONTAINS
  ! Stub. In the real build this lives in cn/spcd_lookup.f90 and reads JTYPE
  ! from the loaded variant config. The caller must ensure cn.json has been
  ! parsed so JTYPE is populated.
  PURE INTEGER FUNCTION SPCD_TO_INDEX(SPCD)
    INTEGER, INTENT(IN) :: SPCD
    INTEGER :: J
    INCLUDE 'COEFFS.f90'
    SPCD_TO_INDEX = 0
    DO J = 1, MAXSP
       IF (NINT(JTYPE(J)) == SPCD) THEN
          SPCD_TO_INDEX = J
          RETURN
       END IF
    END DO
  END FUNCTION SPCD_TO_INDEX

END SUBROUTINE DGF_R


!------------------------------------------------------------------------------
! HTGF_R: vectorised Height Growth Function wrapper
!------------------------------------------------------------------------------
SUBROUTINE HTGF_R(NTREES, ISPCD_IN, HT_IN, CR_IN, CCFL_IN, CCH_IN, &
                  ELEV_IN, TD_IN, EMT_IN, NYRS, HT_OUT, IERR)
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: NTREES
  INTEGER, INTENT(IN)  :: ISPCD_IN(NTREES)
  REAL,    INTENT(IN)  :: HT_IN(NTREES)
  REAL,    INTENT(IN)  :: CR_IN(NTREES)
  REAL,    INTENT(IN)  :: CCFL_IN(NTREES)
  REAL,    INTENT(IN)  :: CCH_IN(NTREES)
  REAL,    INTENT(IN)  :: ELEV_IN
  REAL,    INTENT(IN)  :: TD_IN
  REAL,    INTENT(IN)  :: EMT_IN
  INTEGER, INTENT(IN)  :: NYRS
  REAL,    INTENT(OUT) :: HT_OUT(NTREES)
  INTEGER, INTENT(OUT) :: IERR

  INCLUDE 'PRGPRM.f90'
  INCLUDE 'CALCOM.f90'
  INCLUDE 'ARRAYS.f90'
  INCLUDE 'COEFFS.f90'
  INCLUDE 'CONTRL.f90'
  INCLUDE 'OUTCOM.f90'
  INCLUDE 'PLOT.f90'
  INCLUDE 'MULTCM.f90'
  INCLUDE 'HTCAL.f90'
  INCLUDE 'VARCOM.f90'

  INTEGER :: I, IY

  IERR = 0
  IF (NTREES > MAXTRE) THEN
     IERR = 1
     RETURN
  END IF

  ELEV = ELEV_IN
  ICYC = 1

  DO I = 1, NTREES
     ISP(I)   = ISPCD_IN(I)   ! same caveat as DGF_R: SPCD_TO_INDEX in real build
     HT(I)    = HT_IN(I)
     ICR(I)   = INT(CR_IN(I) * 100.0)
     PCT(I)   = CCFL_IN(I)
     PROB(I)  = 1.0
     ITRE(I)  = I
  END DO

  FINT = 1.0
  DO IY = 1, NYRS
     CALL HTGF
     DO I = 1, NTREES
        HT(I) = HT(I) + HTG(I)
     END DO
  END DO

  DO I = 1, NTREES
     HT_OUT(I) = HT(I)
  END DO

  RETURN
END SUBROUTINE HTGF_R
