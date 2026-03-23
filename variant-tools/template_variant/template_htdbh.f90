! ============================================================================
! TEMPLATE: Height-Diameter Relationship for FVS Variant
! ============================================================================
!
! This module implements height-diameter relationships used to:
! 1. Estimate missing heights from diameter measurements (CRATET)
! 2. Estimate missing diameters from height measurements (REGENT)
!
! EQUATION FORM (Curtis-Arney):
!   H = 4.5 + exp(b0 + b1/DBH^b2)
!
! OR (Alternative Weibull form):
!   H = HMAX * (1 - exp(-k * DBH^c))
!
! WHERE:
!   H     = Total height from ground to tree tip (feet)
!   DBH   = Diameter at breast height (inches)
!   HMAX  = Asymptotic maximum height (site-dependent)
!   b0, b1, b2, k, c = Species and forest-specific coefficients
!
! ============================================================================

module height_diameter
  implicit none
  private
  public :: height_from_diameter, diameter_from_height, htdbh_constants

  ! ======================================================================
  ! Height-Diameter Coefficients
  ! ======================================================================
  ! Three parameters per species for Curtis-Arney equation
  ! COEFFS(1,ispc) = b0 (intercept)
  ! COEFFS(2,ispc) = b1 (diameter scaling factor)
  ! COEFFS(3,ispc) = b2 (diameter exponent)
  !
  ! TODO: Replace these with coefficients from your variant calibration
  ! Typically fit to FIA plot data using non-linear regression

  real, parameter :: COEFFS(3, 108) = reshape([ &
    ! Species 1-10 coefficients (b0, b1, b2 for each)
    -2.53, 15.4, 1.05, &   ! Species 1
    -1.92, 12.1, 1.00, &   ! Species 2
    -2.10, 14.5, 1.08, &   ! Species 3
    -2.05, 13.8, 1.03, &   ! Species 4
    -2.05, 13.8, 1.03, &   ! Species 5
    -2.05, 13.8, 1.03, &   ! Species 6
    -2.05, 13.8, 1.03, &   ! Species 7
    -1.85, 11.2, 0.95, &   ! Species 8
    -2.45, 14.9, 1.10, &   ! Species 9
    -2.15, 13.2, 1.00, &   ! Species 10
    ! Species 11-20
    -2.30, 14.0, 1.08, &   ! Species 11
    -1.95, 12.8, 0.98, &   ! Species 12
    -1.95, 12.8, 0.98, &   ! Species 13
    -1.95, 12.8, 0.98, &   ! Species 14
    -1.95, 12.8, 0.98, &   ! Species 15
    -2.25, 13.5, 1.05, &   ! Species 16
    -2.25, 13.5, 1.05, &   ! Species 17
    -2.15, 13.2, 1.00, &   ! Species 18
    -2.15, 13.2, 1.00, &   ! Species 19
    -2.15, 13.2, 1.00, &   ! Species 20
    ! Species 21-30 (continuing pattern)
    -2.15, 13.2, 1.00, &   ! Species 21
    -2.15, 13.2, 1.00, &   ! Species 22
    -2.15, 13.2, 1.00, &   ! Species 23
    -2.05, 13.8, 1.03, &   ! Species 24
    -2.15, 13.2, 1.00, &   ! Species 25
    -1.95, 12.5, 0.95, &   ! Species 26
    -2.35, 14.2, 1.08, &   ! Species 27
    -2.20, 13.5, 1.03, &   ! Species 28
    -2.05, 13.0, 0.98, &   ! Species 29
    -2.15, 13.2, 1.00, &   ! Species 30
    ! Repeat pattern for remaining 78 species (species 31-108)
    ! TODO: Fill in actual coefficients for all 108 species
    -2.15, 13.2, 1.00, &   ! Species 31-108 (PLACEHOLDER VALUES)
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00, &
    -2.15, 13.2, 1.00 &
  ], shape=[3, 108])

contains

  subroutine height_from_diameter(ispc, dbh, height)
    ! ======================================================================
    ! Estimate total height from diameter
    !
    ! Uses Curtis-Arney equation:
    !   H = 4.5 + exp(b0 + b1/DBH^b2)
    !
    ! Arguments:
    !   ispc (in)   = Species code (1-MAXSP)
    !   dbh  (in)   = Diameter at breast height (inches)
    !   height (out) = Estimated total height (feet)
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: dbh
    real, intent(out) :: height
    real :: b0, b1, b2, dbh_safe

    ! Guard against division by zero
    dbh_safe = max(0.1, dbh)

    ! Extract species-specific coefficients
    b0 = COEFFS(1, ispc)
    b1 = COEFFS(2, ispc)
    b2 = COEFFS(3, ispc)

    ! Apply Curtis-Arney equation
    height = 4.5 + exp(b0 + b1 / (dbh_safe ** b2))

    ! Bound to realistic range (minimum 4.5 ft, maximum reasonable height)
    height = max(4.5, height)
    height = min(250.0, height)

  end subroutine height_from_diameter

  subroutine diameter_from_height(ispc, height, dbh)
    ! ======================================================================
    ! Estimate diameter from height (inverse of Curtis-Arney equation)
    !
    ! Solves for DBH given H using Newton-Raphson iteration or algebraically
    ! if possible. For Curtis-Arney, this requires iterative solution.
    !
    ! Arguments:
    !   ispc (in)  = Species code (1-MAXSP)
    !   height (in) = Total tree height (feet)
    !   dbh (out)   = Estimated diameter at breast height (inches)
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: height
    real, intent(out) :: dbh
    real :: b0, b1, b2
    real :: h_safe, dbh_try, h_try, error, derivative
    integer :: iter

    ! Guard against invalid heights
    h_safe = max(4.5, min(250.0, height))

    ! Extract coefficients
    b0 = COEFFS(1, ispc)
    b1 = COEFFS(2, ispc)
    b2 = COEFFS(3, ispc)

    ! Initial guess: assume linear relationship with typical slope
    dbh = (h_safe - 4.5) * 0.3

    ! Newton-Raphson iteration to solve: h_safe = 4.5 + exp(b0 + b1/dbh^b2)
    do iter = 1, 10
      dbh = max(0.1, dbh)  ! Prevent negative/zero diameter

      ! Current height estimate
      h_try = 4.5 + exp(b0 + b1 / (dbh ** b2))

      ! Residual (error in height)
      error = h_try - h_safe

      ! If error is small enough, converged
      if (abs(error) < 0.01) exit

      ! Derivative of h with respect to dbh (for Newton's method)
      ! d/ddbh[4.5 + exp(b0 + b1/dbh^b2)] = -exp(b0 + b1/dbh^b2) * b1 * b2 / dbh^(b2+1)
      derivative = -exp(b0 + b1 / (dbh ** b2)) * b1 * b2 / (dbh ** (b2 + 1.0))

      ! Update diameter estimate
      if (abs(derivative) > 0.0001) then
        dbh = dbh - error / derivative
      else
        ! If derivative is near zero, use bisection fallback
        dbh = dbh * 0.99
      end if
    end do

    ! Bound to reasonable range
    dbh = max(0.1, min(100.0, dbh))

  end subroutine diameter_from_height

  subroutine htdbh_constants()
    ! ======================================================================
    ! Initialize height-diameter model constants
    ! Called once at the start of simulation
    !
    ! TODO: Add any variant-specific calibration or adjustment logic
    ! ======================================================================

    ! Currently all coefficients are defined as parameters
    ! Modify this if your variant needs dynamic calibration

  end subroutine htdbh_constants

end module height_diameter
