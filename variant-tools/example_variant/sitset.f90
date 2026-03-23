! ============================================================================
! TEMPLATE: Site Index Setup for FVS Variant
! ============================================================================
!
! This module initializes site index values for each species when the user
! does not provide them via keywords. Site index (SI) is a measure of site
! productivity, typically expressed as the height (in feet) that a dominant
! tree would reach at a reference age (usually 50 years).
!
! Site index is used in diameter growth and height growth models to capture
! site productivity differences. Higher site index = more productive site.
!
! EQUATION FORMS:
! 1. Site index conversion tables (most common for FVS)
!    * Convert from one species' SI to another (e.g., red oak SI to oak group SI)
!    * Matrix lookup tables with interpolation
!
! 2. SI curves by forest type (e.g., different curves for hardwood vs. softwood)
!
! 3. Polynomial equations relating SI to site variables (elevation, aspect, etc.)
!
! ============================================================================

module site_index
  implicit none

  integer, parameter :: MAXSP = 108

  ! ======================================================================
  ! STAND DENSITY INDEX (SDI) MAXIMUM VALUES BY SPECIES
  ! ======================================================================
  ! SDICON(ispc) = maximum self-thinning density index for this species
  ! Used to model density-dependent mortality and growth reduction
  !
  ! Typical range: 150 - 900 (species with different ecological niches)
  ! - High SDI (>700): small-crowned species (many trees per unit area)
  ! - Medium SDI (400-700): typical forest trees
  ! - Low SDI (<400): large-crowned trees (fewer trees per unit area)
  !
  ! TODO: Obtain species-specific SDI values from:
  !   - Curtis and Reukema (1970) "Maximum density stocking index"
  !   - Regional forest mensuration studies
  !   - Published SDI tables for your variant's species

  real :: SDICON(MAXSP) = [ &
    655.0, 387.0, 412.0, 506.0, 412.0, 500.0, 412.0, 505.0, 529.0, 480.0, &  ! 1-10
    499.0, 771.0, 771.0, 354.0, 354.0, 510.0, 518.0, 529.0, 356.0, 490.0, &  ! 11-20
    398.0, 398.0, 310.0, 408.0, 354.0, 421.0, 371.0, 371.0, 590.0, 369.0, &  ! 21-30
    350.0, 400.0, 466.0, 466.0, 302.0, 276.0, 302.0, 302.0, 230.0, 364.0, &  ! 31-40
    414.0, 408.0, 423.0, 414.0, 408.0, 478.0, 430.0, 415.0, 562.0, 384.0, &  ! 41-50
    648.0, 520.0, 648.0, 384.0, 361.0, 423.0, 336.0, 311.0, 361.0, 315.0, &  ! 51-60
    370.0, 365.0, 455.0, 417.0, 361.0, 336.0, 414.0, 342.0, 370.0, 405.0, &  ! 61-70
    492.0, 371.0, 371.0, 400.0, 420.0, 147.0, 155.0, 283.0, 283.0, 404.0, &  ! 71-80
    492.0, 492.0, 422.0, 726.0, 430.0, 164.0, 164.0, 499.0, 315.0, 343.0, &  ! 81-90
    447.0, 492.0, 526.0, 526.0, 282.0, 282.0, 227.0, 257.0, 344.0, 243.0, &  ! 91-100
    343.0, 657.0, 375.0, 257.0, 463.0, 304.0, 463.0, 463.0 &                  ! 101-108
  ]

contains

  subroutine initialize_site_index_defaults(sitear, isp, nsp)
    ! ======================================================================
    ! Set default site index for species that don't have one specified by user
    !
    ! Arguments:
    !   sitear (out) = Array of site index values by species
    !   isp (in)     = Array of species codes for trees in inventory
    !   nsp (in)     = Number of trees
    !
    ! TODO: Modify this subroutine to:
    ! 1. Apply your variant's specific default SI values
    ! 2. Potentially use site variables (elevation, aspect, soil) if available
    ! 3. Implement forest type-specific SI curves
    ! ======================================================================
    real, intent(out) :: sitear(:)
    integer, intent(in) :: isp(:)
    integer, intent(in) :: nsp
    integer :: i, ispc

    ! Default site index by species
    ! TODO: Replace with values appropriate for your variant
    ! Typical: 40-80 feet at 50 years, depending on species productivity
    real :: default_si(MAXSP)

    ! Initialize default SI array
    default_si = [ &
      60.0, 50.0, 65.0, 70.0, 65.0, 55.0, 65.0, 70.0, 75.0, 70.0, &  ! 1-10
      60.0, 55.0, 55.0, 55.0, 55.0, 70.0, 70.0, 60.0, 60.0, 60.0, &  ! 11-20
      60.0, 60.0, 60.0, 60.0, 60.0, 70.0, 80.0, 75.0, 70.0, 65.0, &  ! 21-30
      60.0, 60.0, 60.0, 60.0, 70.0, 65.0, 75.0, 70.0, 60.0, 70.0, &  ! 31-40
      60.0, 60.0, 60.0, 70.0, 65.0, 60.0, 60.0, 65.0, 50.0, 50.0, &  ! 41-50
      50.0, 50.0, 60.0, 65.0, 70.0, 70.0, 60.0, 60.0, 60.0, 50.0, &  ! 51-60
      60.0, 60.0, 60.0, 70.0, 70.0, 60.0, 70.0, 70.0, 70.0, 60.0, &  ! 61-70
      60.0, 70.0, 70.0, 60.0, 70.0, 75.0, 75.0, 60.0, 60.0, 60.0, &  ! 71-80
      70.0, 65.0, 65.0, 60.0, 60.0, 70.0, 60.0, 65.0, 60.0, 50.0, &  ! 81-90
      50.0, 60.0, 70.0, 70.0, 65.0, 65.0, 70.0, 60.0, 70.0, 75.0, &  ! 91-100
      60.0, 65.0, 75.0, 75.0, 60.0, 70.0, 60.0, 50.0 &               ! 101-108
    ]

    ! Assign site index to all species with unspecified values
    do i = 1, nsp
      if (sitear(isp(i)) <= 0.0) then
        sitear(isp(i)) = default_si(isp(i))
      end if
    end do

  end subroutine initialize_site_index_defaults

  subroutine site_index_conversion(si_from, sp_from, sp_to, si_to)
    ! ======================================================================
    ! Convert site index from one species to another
    !
    ! This is used when users provide SI for one species but the model
    ! needs SI for a different species (e.g., red oak SI to white oak SI)
    !
    ! Arguments:
    !   si_from (in)  = Input site index (feet at base age 50)
    !   sp_from (in)  = Source species code
    !   sp_to (in)    = Target species code
    !   si_to (out)   = Converted site index
    !
    ! TODO: Implement your variant's SI conversion tables
    ! Common approaches:
    ! 1. Direct lookup tables (if available in literature)
    ! 2. Linear regression equations
    ! 3. Group conversion (all hardwoods use oak SI standard)
    ! ======================================================================
    real, intent(in) :: si_from
    integer, intent(in) :: sp_from, sp_to
    real, intent(out) :: si_to

    ! Placeholder: return input SI unchanged
    ! TODO: Replace with actual conversion logic

    if (sp_from == sp_to) then
      si_to = si_from
    else
      ! TODO: Implement species-pair-specific conversion equations
      ! Example linear conversion:
      ! si_to = a + b * si_from
      ! where a, b depend on sp_from and sp_to

      ! For now, simple scaling based on productivity differences
      si_to = si_from
    end if

  end subroutine site_index_conversion

  subroutine si_from_site_variables(elevation, aspect, slope, soil_class, &
                                     species, si)
    ! ======================================================================
    ! Estimate site index from physical site variables
    !
    ! This allows SI to be estimated from elevation, aspect, and soil
    ! when direct measurements are not available.
    !
    ! Arguments:
    !   elevation (in)  = Elevation (feet)
    !   aspect (in)     = Aspect (degrees, 0=N, 90=E, 180=S, 270=W)
    !   slope (in)      = Slope steepness (percent)
    !   soil_class (in) = Soil productivity class (e.g., 1=best, 3=worst)
    !   species (in)    = Species code
    !   si (out)        = Estimated site index
    !
    ! TODO: Develop or obtain equations relating SI to site variables
    ! for your variant. Sources:
    ! * Regional forest productivity studies
    ! * Soil-vegetation relationships for your region
    ! * Published SI curves by forest type
    ! ======================================================================
    real, intent(in) :: elevation, aspect, slope
    integer, intent(in) :: soil_class, species
    real, intent(out) :: si
    real :: elev_factor, soil_factor, aspect_factor

    ! Placeholder relationships (TODO: Replace with calibrated values)

    ! Elevation effect: SI typically decreases with elevation
    elev_factor = 60.0 - (elevation - 1000.0) * 0.01

    ! Soil effect: SI increases with soil productivity
    select case (soil_class)
    case (1)
      soil_factor = 1.15
    case (2)
      soil_factor = 1.00
    case (3)
      soil_factor = 0.85
    case default
      soil_factor = 1.00
    end select

    ! Aspect effect: South-facing slopes often more productive
    ! (0.5 at north-facing, 1.2 at south-facing for some species)
    aspect_factor = 0.9 + 0.003 * abs(aspect - 180.0)

    ! Combine factors
    si = elev_factor * soil_factor * aspect_factor

    ! Bound to realistic range
    si = max(20.0, min(100.0, si))

  end subroutine si_from_site_variables

  subroutine sdindex_from_si(si, species, sdindex)
    ! ======================================================================
    ! Calculate stand density index (SDI) from site index
    ! Some models correlate SDI with SI (better sites support higher density)
    !
    ! Arguments:
    !   si (in)        = Site index for the species
    !   species (in)   = Species code
    !   sdindex (out)  = Stand density index
    ! ======================================================================
    real, intent(in) :: si
    integer, intent(in) :: species
    real, intent(out) :: sdindex

    ! Placeholder: use species' maximum SDI as constant
    if (species >= 1 .and. species <= MAXSP) then
      sdindex = SDICON(species)
    else
      sdindex = 400.0  ! Default value
    end if

    ! TODO: Apply site-dependent modifiers if desired
    ! Some species show SI-SDI relationships (e.g., better sites allow denser stands)

  end subroutine sdindex_from_si

end module site_index
