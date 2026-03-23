! ============================================================================
! TEMPLATE: Mortality Model for FVS Variant
! ============================================================================
!
! This module implements the species-specific mortality (survival) model.
! The model predicts the probability that a tree will survive the growth period.
!
! GENERAL EQUATION FORM:
!   Survival Probability = f(DBH, relative height, species shade tolerance)
!
! The FVS framework uses multiple approaches:
! 1. Density-related mortality based on stand density and tree size
! 2. Background (natural) mortality as a baseline rate
! 3. Species-specific stress tolerance factors
!
! SHADE TOLERANCE ARRAY:
!   VARADJ(ispc) ranges from 0.0 (shade intolerant) to 1.0 (very tolerant)
!   This factor modulates how much a tree's position in the canopy affects
!   its mortality risk. Intolerant species (low VARADJ) suffer high mortality
!   when overtopped; tolerant species (high VARADJ) survive better under shade.
!
! ============================================================================

module mortality_model
  implicit none
  private
  public :: background_mortality, compute_survival, mortality_constants

  ! ======================================================================
  ! Species Shade Tolerance Array
  ! ======================================================================
  ! Each value represents the shade tolerance for one FIA species
  ! Interpretation:
  !   < 0.3 : Very intolerant (needs full sunlight, suffers under shade)
  !   0.3-0.5 : Intolerant (prefers sunlight, reduced growth/survival in shade)
  !   0.5-0.7 : Intermediate (moderate tolerance, can survive moderate shade)
  !   0.7-0.9 : Tolerant (prefers shade, survives well under overtopping)
  !   > 0.9 : Very tolerant (shade specialist, thrives with overtopping)
  !
  ! TODO: Replace these values with shade tolerance ratings appropriate
  ! for your variant's species. Sources include:
  !   - USDA Forest Service shade tolerance guides
  !   - Regional forestry manuals
  !   - Expert opinion from regional ecologists
  !

  real, parameter :: VARADJ(108) = [ &
    0.90, 0.10, 0.50, 0.80, 0.50, 0.70, 0.50, 0.30, 0.50, 0.30, &  ! Species 1-10
    0.30, 0.70, 0.50, 0.20, 0.50, 0.70, 0.90, 0.30, 0.30, 0.30, &  ! Species 11-20
    0.30, 0.30, 0.30, 0.30, 0.30, 0.85, 0.90, 0.10, 0.70, 0.50, &  ! Species 21-30
    0.30, 0.30, 0.30, 0.30, 0.50, 0.50, 0.90, 0.50, 0.30, 0.70, &  ! Species 31-40
    0.30, 0.30, 0.30, 0.70, 0.50, 0.30, 0.30, 0.50, 0.10, 0.10, &  ! Species 41-50
    0.10, 0.10, 0.30, 0.40, 0.50, 0.50, 0.30, 0.30, 0.30, 0.10, &  ! Species 51-60
    0.30, 0.30, 0.30, 0.50, 0.50, 0.30, 0.50, 0.50, 0.50, 0.30, &  ! Species 61-70
    0.30, 0.70, 0.70, 0.30, 0.50, 0.90, 0.90, 0.30, 0.30, 0.30, &  ! Species 71-80
    0.70, 0.50, 0.40, 0.30, 0.30, 0.70, 0.30, 0.50, 0.30, 0.10, &  ! Species 81-90
    0.10, 0.30, 0.70, 0.70, 0.50, 0.50, 0.70, 0.30, 0.70, 0.90, &  ! Species 91-100
    0.30, 0.50, 0.90, 0.90, 0.30, 0.70, 0.30, 0.10 &                ! Species 101-108
  ]

  ! ======================================================================
  ! Background Mortality Coefficients
  ! ======================================================================
  ! These model the baseline mortality probability independent of density
  ! Typically a logistic or Weibull survival function
  !
  ! TODO: Add coefficients from your variant's calibration data
  ! Common equation forms:
  !   1. Logistic: S = 1 / (1 + exp(b0 + b1*ln(DBH) + b2*BA))
  !   2. Weibull:  S = exp(-(lambda * DBH^k))
  !   3. Exponential: S = exp(b0 + b1*DBH + b2*DBH^2)

  ! Background mortality rate (annual probability of death)
  ! Typical values: 0.005 - 0.05 (0.5% - 5% annual mortality)
  real, parameter :: MORT_B0(108) = -4.5  ! TODO: Replace with species-specific values
  real, parameter :: MORT_B1(108) = -0.3
  real, parameter :: MORT_B2(108) = -0.02

contains

  subroutine background_mortality(ispc, dbh, years, survival_prob)
    ! ======================================================================
    ! Calculate background (natural) mortality probability
    !
    ! Arguments:
    !   ispc (in)          = Species code (1-MAXSP)
    !   dbh  (in)          = Diameter at breast height (inches)
    !   years (in)         = Growth period (years)
    !   survival_prob (out) = Probability of survival (0.0 - 1.0)
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: dbh, years
    real, intent(out) :: survival_prob
    real :: annual_mort, annual_survival

    ! Calculate annual mortality probability
    ! TODO: Replace with your variant's background mortality equation
    ! Example logistic form:
    annual_mort = 1.0 / (1.0 + exp(-(MORT_B0(ispc) + MORT_B1(ispc) * log(max(dbh, 0.1)))))

    ! Bound to realistic range
    annual_mort = max(0.0001, min(0.2, annual_mort))

    ! Convert to multi-year survival (assuming constant annual rate)
    annual_survival = 1.0 - annual_mort
    survival_prob = annual_survival ** years

    ! Ensure output is in valid range
    survival_prob = max(0.0, min(1.0, survival_prob))

  end subroutine background_mortality

  subroutine compute_survival(ispc, dbh, relative_height, ba, years, survival_prob, debug)
    ! ======================================================================
    ! Compute overall survival probability combining density and background
    ! components
    !
    ! Arguments:
    !   ispc (in)          = Species code (1-MAXSP)
    !   dbh  (in)          = Diameter at breast height (inches)
    !   relative_height (in) = Height / average stand height (0.0 - 1.0)
    !   ba   (in)          = Stand basal area (sq ft/acre)
    !   years (in)         = Growth period (years)
    !   survival_prob (out) = Probability of survival (0.0 - 1.0)
    !   debug (in)         = Whether to print debug output
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: dbh, relative_height, ba, years
    real, intent(out) :: survival_prob
    logical, intent(in), optional :: debug
    real :: back_mort, density_mort, peff
    real :: relhta

    ! Get background mortality component
    call background_mortality(ispc, dbh, years, survival_prob)

    ! Calculate density-related mortality efficiency
    ! Trees are ranked by relative height; taller trees are less likely to die
    relhta = relative_height * 100.0
    if (relhta > 100.0) relhta = 100.0
    if (relhta < 1.0) relhta = 1.0

    ! Empirical relationship between relative height and survival
    ! PEFF ranges from 0.01 to 1.0
    peff = 0.84525 - 0.01074 * relhta + 0.0000002 * (relhta ** 3.0)
    if (peff > 1.0) peff = 1.0
    if (peff < 0.01) peff = 0.01

    ! Combine with shade tolerance (VARADJ controls mortality risk in understory)
    ! High VARADJ (shade tolerant) = lower mortality in shade
    ! Low VARADJ (shade intolerant) = higher mortality in shade
    density_mort = peff * (1.0 - VARADJ(ispc)) * 0.1 * years

    ! TODO: Apply stand density effects if needed
    ! For example, normalize by stand density index or relative density

    ! Combine background and density components (multiplicative)
    survival_prob = survival_prob * (1.0 - density_mort)

    ! Bound to valid range
    survival_prob = max(0.0, min(1.0, survival_prob))

    if (present(debug)) then
      if (debug) then
        print *, 'MORTALITY: ISPC=', ispc, ' DBH=', dbh, ' REL_HT=', relative_height
        print *, '  PEFF=', peff, ' VARADJ=', VARADJ(ispc)
        print *, '  SURVIVAL=', survival_prob
      end if
    end if

  end subroutine compute_survival

  subroutine mortality_constants()
    ! ======================================================================
    ! Initialize mortality model constants
    ! This subroutine is called once at the start of each growth cycle
    !
    ! TODO: Add any site-dependent or dynamic adjustment logic here
    ! ======================================================================

    ! Currently all coefficients are defined as parameters
    ! Modify this subroutine if your variant needs dynamic calibration

  end subroutine mortality_constants

end module mortality_model
