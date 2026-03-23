! ============================================================================
! TEMPLATE: Diameter Growth Function (DGF) for FVS Variant
! ============================================================================
!
! This module implements the diameter growth prediction model. Diameter growth
! is predicted from diameter at breast height (DBH), site index, and basal area
! in larger trees (BAL).
!
! EQUATION FORM (for change in squared diameter):
!   ln(DDS) = b0 + b1*ln(DBH) + b2*DBH^2 + b3*ln(BA) + COR(ispc)
!
! WHERE:
!   DDS   = Change in squared diameter (derived from DG over 5 years)
!   DBH   = Diameter at breast height (inches)
!   BA    = Basal area of larger trees (square feet per acre)
!   SITE  = Site index (base age 50, feet)
!   b0-b3 = Species-specific coefficients
!   COR   = Species-specific bias correction
!
! The model uses an iterative approach over annual timesteps because
! the growth equation is specified on an annual basis.
!
! ============================================================================

module diameter_growth
  implicit none
  private
  public :: diameter_growth_increment, dg_constants

  ! Species-specific diameter growth coefficients
  ! These relate potential basal area growth to site index and diameter
  !
  ! TODO: Replace with coefficients from your variant's calibration data
  ! Each species gets one coefficient for site index sensitivity (B1)
  ! and one for diameter sensitivity (B2)

  real, parameter :: B1(108) = [ &
    ! B1: Site index sensitivity in basal area growth equation
    ! Typical range: 0.0005 - 0.0015
    ! TODO: Higher values = more responsive to site quality
    0.0008829, 0.0009933, 0.0008721, 0.0008236, 0.0008236, 0.0008236, 0.0008236, 0.0009252, &
    0.0011303, 0.0009252, 0.0006634, 0.0009050, 0.0009050, 0.0009050, 0.0009050, 0.0008737, &
    0.0008737, 0.0006634, 0.0006634, 0.0006634, 0.0006634, 0.0006634, 0.0006634, 0.0006634, &
    0.0006634, 0.0007906, 0.0007439, 0.0007439, 0.0007439, 0.0006668, 0.0006668, 0.0006668, &
    0.0009766, 0.0009766, 0.0007993, 0.0007993, 0.0007993, 0.0007993, 0.0007993, 0.0006911, &
    0.0008992, 0.0008992, 0.0008992, 0.0008992, 0.0008992, 0.0008815, 0.0008815, 0.0008815, &
    0.0011885, 0.0011885, 0.0011885, 0.0011885, 0.0011885, 0.0007929, 0.0007417, 0.0007417, &
    0.0007417, 0.0007417, 0.0007417, 0.0008769, 0.0008769, 0.0008769, 0.0008769, 0.0008238, &
    0.0008238, 0.0008238, 0.0008920, 0.0008920, 0.0008550, 0.0008550, 0.0009567, 0.0009567, &
    0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, &
    0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, &
    0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0009567, 0.0003604, &
    0.0003604, 0.0003604, 0.0003604, 0.0003604, 0.0003604, 0.0003604, 0.0003604, 0.0003604, &
    0.0003604, 0.0003604, 0.0003604 &
  ]

  real, parameter :: B2(108) = [ &
    ! B2: Diameter sensitivity in basal area growth equation
    ! Typical range: 0.03 - 0.15
    ! TODO: Controls the asymptotic diameter response (how growth slows with size)
    0.0602785, 0.0816995, 0.0578650, 0.0549439, 0.0549439, 0.0549439, 0.0549439, 0.1134195, &
    0.0934796, 0.1134195, 0.1083470, 0.0517297, 0.0517297, 0.0517297, 0.0517297, 0.0940538, &
    0.0940538, 0.1083470, 0.1083470, 0.1083470, 0.1083470, 0.1083470, 0.1083470, 0.1083470, &
    0.1083470, 0.0651982, 0.0706905, 0.0706905, 0.0706905, 0.0768212, 0.0768212, 0.0768212, &
    0.0832328, 0.0832328, 0.0779654, 0.0779654, 0.0779654, 0.0779654, 0.0779654, 0.0730441, &
    0.0925395, 0.0925395, 0.0925395, 0.0925395, 0.0925395, 0.1419212, 0.1419212, 0.1419212, &
    0.0920050, 0.0920050, 0.0920050, 0.0920050, 0.0920050, 0.1568904, 0.0867535, 0.0867535, &
    0.0867535, 0.0867535, 0.0867535, 0.0866621, 0.0866621, 0.0866621, 0.0866621, 0.0790660, &
    0.0790660, 0.0790660, 0.0979702, 0.0979702, 0.0957964, 0.0957964, 0.1038458, 0.1038458, &
    0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, &
    0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, &
    0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.1038458, 0.0328767, &
    0.0328767, 0.0328767, 0.0328767, 0.0328767, 0.0328767, 0.0328767, 0.0328767, 0.0328767, &
    0.0328767, 0.0328767, 0.0328767 &
  ]

  ! Bias correction factors applied to ln(DDS)
  ! TODO: Add COR values from your model calibration
  real, parameter :: COR(108) = 0.0  ! TODO: Replace with species-specific values

  ! Diameter growth modifier arrays
  ! These would be set by the dg_constants() subroutine
  real :: DGCON(108)   ! Diameter growth constant adjustment
  real :: ATTEN(108)   ! Attenuation factor for density effects
  real :: SMCON(108)   ! Small tree constant

contains

  subroutine diameter_growth_increment(ispc, dbh, sitear, ba, years, diagr)
    ! ======================================================================
    ! Calculate the diameter growth increment for a single tree
    !
    ! Arguments:
    !   ispc  (in)  = Species code (1-MAXSP)
    !   dbh   (in)  = Current diameter at breast height (inches)
    !   sitear (in) = Site index for the species (feet, base age 50)
    !   ba    (in)  = Basal area of trees larger than target (sq ft/acre)
    !   years (in)  = Number of years to project (typically 5)
    !   diagr (out) = Diameter growth (outside bark, inches)
    ! ======================================================================
    integer, intent(in) :: ispc
    real, intent(in) :: dbh, sitear, ba, years
    real, intent(out) :: diagr
    real :: potbag, bagmod, deld, qtrba, qdbh, dds, temd
    real :: dgb1, dgb2
    integer :: iloop

    ! Initialize variables
    temd = dbh
    diagr = 0.0

    ! Retrieve species-specific coefficients
    dgb1 = B1(ispc)
    dgb2 = B2(ispc)

    ! Iterate for annual timesteps (if years > 1, loop multiple times)
    do iloop = 1, int(years)
      ! Skip if diameter is zero or negative
      if (temd <= 0.0) exit

      ! Calculate potential basal area growth using site index and diameter
      ! POTBAG = b1 * SITEAR * (1 - exp(-b2 * DBH))
      potbag = dgb1 * sitear * (1.0 - exp(-dgb2 * temd))

      ! Apply calibration factor (typically 0.7)
      potbag = potbag * 0.7

      ! Get basal area modifier (accounting for competition from larger trees)
      ! TODO: Call balmod() function or apply your variant's competition logic
      bagmod = 1.0  ! PLACEHOLDER: Replace with actual BAL modifier

      ! Calculate annual diameter increment
      deld = potbag * bagmod

      ! Convert diameter growth to squared diameter change
      ! Using the conversion: if DG adds DELD to basal area growth,
      ! then QDB = sqrt(QBA / 0.0054542) where QBA = (D^2)*0.0054542 + DELD
      qtrba = deld + (temd * temd * 0.0054542)
      qdbh = sqrt(qtrba / 0.0054542)

      ! Update diameter for next iteration
      temd = qdbh
    end do

    ! Calculate final diameter growth
    diagr = temd - dbh

    ! Bound the growth to prevent unrealistic values
    if (diagr < 0.0001) diagr = 0.0001
    if (diagr > (dbh * 0.5)) diagr = dbh * 0.5  ! Max 50% growth per period

  end subroutine diameter_growth_increment

  subroutine dg_constants()
    ! ======================================================================
    ! Initialize diameter growth model constants
    ! This subroutine is called once at the start of each growth cycle
    ! to set any site-dependent or dynamic coefficients.
    !
    ! TODO: Add any variant-specific initialization logic here
    ! ======================================================================
    integer :: ispc

    ! Initialize arrays
    do ispc = 1, 108
      DGCON(ispc) = 0.0
      ATTEN(ispc) = 1000.0
      SMCON(ispc) = 0.0
    end do

    ! TODO: Add any site-dependent adjustments here
    ! For example, you might apply regional or forest-specific modifiers

  end subroutine dg_constants

end module diameter_growth
