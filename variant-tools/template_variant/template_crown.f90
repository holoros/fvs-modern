! ============================================================================
! TEMPLATE: Crown Ratio Model for FVS Variant
! ============================================================================
!
! This module implements the crown ratio prediction model. Crown ratio is
! the ratio of crown length to total tree height, expressed as a percentage.
! The model predicts crown ratio from basal area and tree diameter.
!
! EQUATION FORM:
!   CR = 10 * (BCR1/(1 + BCR2*BA) + BCR3*(1 - exp(BCR4*DBH)))
!
! WHERE:
!   CR    = Predicted crown ratio (percentage, 1-95)
!   BCR1  = Intercept coefficient (species-specific)
!   BCR2  = Basal area sensitivity (species-specific)
!   BCR3  = Asymptotic crown ratio (species-specific)
!   BCR4  = Diameter-dependent shape parameter (species-specific)
!   BA    = Stand basal area (square feet per acre)
!   DBH   = Diameter at breast height (inches)
!
! ============================================================================

module crown_model
  implicit none
  private
  public :: crown_ratio, crown_constants

  ! Species-specific coefficients
  ! TODO: Replace with coefficients from your variant's calibration data
  ! Each row corresponds to one species in your FIA species list

  real, parameter :: BCR1(108) = [ &
    ! Softwoods (conifers)
    5.630, 6.000, 7.840, 5.540, 5.540, 5.540, 5.540, 6.640, 5.350, 6.790, &
    5.710, 5.710, 5.710, 5.710, 4.350, 4.350, 4.350, 4.350, 4.350, 4.350, &
    4.350, 4.350, 4.350, 3.400, 4.180, 4.180, 4.180, 4.180, 4.180, 4.180, &
    5.000, 5.000, 4.000, 4.000, 4.000, 4.000, 4.000, 6.210, 3.733, 3.733, &
    3.733, 3.733, 3.733, 3.733, 3.733, 4.500, 4.500, 4.500, 4.500, 4.500, &
    3.733, 4.111, 4.111, 4.111, 4.111, 4.111, 4.000, 4.000, 4.000, 4.000, &
    3.733, 3.733, 3.733, 5.840, 5.840, 4.200, 4.200, 4.000, 4.000, 4.000, &
    4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, &
    4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, &
    4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, &
    4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000 &
  ]

  real, parameter :: BCR2(108) = [ &
    ! BCR2: Basal area sensitivity coefficient
    ! TODO: Positive values indicate BA reduces crown ratio (competition effect)
    0.0047, 0.0053, 0.0057, 0.0072, 0.0072, 0.0072, 0.0072, 0.0135, 0.0053, 0.0058, &
    0.0077, 0.0077, 0.0077, 0.0077, 0.0046, 0.0046, 0.0046, 0.0046, 0.0046, 0.0046, &
    0.0046, 0.0046, 0.0046, 0.0066, 0.0025, 0.0025, 0.0025, 0.0025, 0.0025, 0.0025, &
    0.0066, 0.0066, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0073, 0.0040, 0.0040, &
    0.0040, 0.0040, 0.0040, 0.0040, 0.0040, 0.0032, 0.0032, 0.0032, 0.0032, 0.0032, &
    0.0040, 0.0054, 0.0054, 0.0054, 0.0054, 0.0054, 0.0024, 0.0024, 0.0024, 0.0024, &
    0.0040, 0.0040, 0.0040, 0.0082, 0.0082, 0.0016, 0.0016, 0.0024, 0.0024, 0.0024, &
    0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, &
    0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, &
    0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, &
    0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024 &
  ]

  real, parameter :: BCR3(108) = [ &
    ! BCR3: Asymptotic crown ratio at large DBH
    ! TODO: Controls the maximum crown ratio the model predicts
    3.523, 0.431, 1.272, 4.200, 4.200, 4.200, 4.200, 3.200, 1.528, 7.590, &
    2.290, 2.290, 2.290, 2.290, 1.820, 1.820, 1.820, 1.820, 1.820, 1.820, &
    1.820, 1.820, 1.820, 2.870, 1.410, 1.410, 1.410, 1.410, 1.410, 1.410, &
    4.920, 4.920, -2.830, -2.830, -2.830, -2.830, -2.830, 9.990, 3.632, 3.632, &
    3.632, 3.632, 3.632, 3.632, 3.632, 0.795, 0.795, 0.795, 0.795, 0.795, &
    3.632, 1.650, 1.650, 1.650, 1.650, 1.650, -2.830, -2.830, -2.830, -2.830, &
    3.632, 3.632, 3.632, 3.260, 3.260, 2.760, 2.760, -2.830, -2.830, -2.830, &
    -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, &
    -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, &
    -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, &
    -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830, -2.830 &
  ]

  real, parameter :: BCR4(108) = [ &
    ! BCR4: Diameter-dependent shape parameter
    ! TODO: Controls the rate of crown ratio change with diameter
    ! Negative values indicate crown ratio decreases with increasing diameter
    -0.0689, -0.0012, -0.1420, -0.0530, -0.0530, -0.0530, -0.0530, -0.0518, -0.0330, -0.0103, &
    -0.2530, -0.2530, -0.2530, -0.2530, -0.2740, -0.2740, -0.2740, -0.2740, -0.2740, -0.2740, &
    -0.2740, -0.2740, -0.2740, -0.4340, -0.5120, -0.5120, -0.5120, -0.5120, -0.5120, -0.5120, &
    -0.0263, -0.0263, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, -0.0100, -0.0412, -0.0412, &
    -0.0412, -0.0412, -0.0412, -0.0412, -0.0412, -0.1050, -0.1050, -0.1050, -0.1050, -0.1050, &
    -0.0412, -0.1100, -0.1100, -0.1100, -0.1100, -0.1100, 0.0210, 0.0210, 0.0210, 0.0210, &
    -0.0412, -0.0412, -0.0412, -0.0490, -0.0490, -0.0250, -0.0250, 0.0210, 0.0210, 0.0210, &
    0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, &
    0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, &
    0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, &
    0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210, 0.0210 &
  ]

contains

  subroutine crown_ratio(ispc, dbh, ba, cr)
    ! ======================================================================
    ! Calculates the predicted crown ratio for a tree
    !
    ! Arguments:
    !   ispc (in)  = Species code (1-MAXSP)
    !   dbh  (in)  = Diameter at breast height (inches)
    !   ba   (in)  = Stand basal area (sq ft/acre)
    !   cr   (out) = Predicted crown ratio (percentage)
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: dbh, ba
    real, intent(out) :: cr
    real :: den

    ! Avoid division by zero
    den = 1.0 + BCR2(ispc) * ba

    ! Calculate crown ratio using the fitted equation
    cr = 10.0 * (BCR1(ispc) / den + BCR3(ispc) * (1.0 - exp(BCR4(ispc) * dbh)))

    ! Bound the output: crown ratio should be between 1% and 95%
    if (cr < 1.0) cr = 1.0
    if (cr > 95.0) cr = 95.0

  end subroutine crown_ratio

  subroutine crown_constants()
    ! ======================================================================
    ! Initialize crown model constants
    ! This subroutine is called once at the start of each growth cycle
    ! to set any site-dependent or dynamic coefficients.
    !
    ! TODO: Add any site-specific calibration logic here if needed
    ! ======================================================================

    ! Currently all coefficients are static (defined as PARAMETER)
    ! Modify this subroutine if your variant requires dynamic adjustment

  end subroutine crown_constants

end module crown_model
