! ============================================================================
! TEMPLATE: Block Data Initialization Module for FVS Variant
! ============================================================================
!
! This module initializes all species-specific coefficients that are loaded
! once at program startup. These are static data that don't change during
! the simulation.
!
! FIA SPECIES CODES:
! FVS uses FIA (Forest Inventory and Analysis) species codes defined by the
! USDA Forest Service. The codes range from 1-999, but each regional variant
! typically uses a subset of ~100-120 species. The species list must be
! mapped to array indices 1-MAXSP in this file.
!
! TODO: Identify which FIA species are present in your region, obtain their
! codes, and map them to the 1-MAXSP array positions used by your variant.
!
! ============================================================================

module block_data_init
  implicit none

  integer, parameter :: MAXSP = 108   ! Maximum species (from PRGPRM)

  ! ======================================================================
  ! SPECIES-SPECIFIC BARK RATIO COEFFICIENTS
  ! ======================================================================
  ! BKRAT(ispc) = ratio of inside-bark diameter to outside-bark diameter
  ! These are used to convert measured outside-bark DBH to inside-bark values
  ! for volume and growth calculations.
  !
  ! Typical range: 0.85 - 0.98
  ! - Softwoods (pines, spruces, firs): 0.88 - 0.96
  ! - Hardwoods (oaks, maples, etc.): 0.85 - 0.95
  !
  ! TODO: Replace with species-specific bark ratios from:
  !   - Regional forestry publications
  !   - FVS regional variant documentation
  !   - Stem analysis studies for your variant's species

  real :: BKRAT(MAXSP) = [ &
    0.9349, 0.9349, 0.9560, 0.9324, 0.9324, 0.9324, 0.9324, 0.9200, 0.9200, 0.8900, &  ! 1-10
    0.9640, 0.9500, 0.9500, 0.9500, 0.9500, 0.9340, 0.9340, 0.9640, 0.9640, 0.9640, &  ! 11-20
    0.9640, 0.9640, 0.9640, 0.9640, 0.9640, 0.9500, 0.9500, 0.9200, 0.9200, 0.9200, &  ! 21-30
    0.9200, 0.9200, 0.9200, 0.9200, 0.9500, 0.9500, 0.9200, 0.9400, 0.9100, 0.9100, &  ! 31-40
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 41-50
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 51-60
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 61-70
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 71-80
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 81-90
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, &  ! 91-100
    0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100, 0.9100 &                     ! 101-108
  ]

  ! ======================================================================
  ! DIAMETER GROWTH BIAS CORRECTION FACTORS
  ! ======================================================================
  ! COR2(ispc) = bias correction for diameter growth predictions
  ! Applied as: DG_predicted = DG_model * COR2
  !
  ! Typical range: 0.8 - 1.2
  ! Values < 1.0 = model overpredicts growth (reduce predictions)
  ! Values > 1.0 = model underpredicts growth (increase predictions)
  !
  ! TODO: Obtain from model calibration against independent test data

  real :: COR2(MAXSP) = 1.0  ! TODO: Replace with species-specific values

  ! ======================================================================
  ! HEIGHT GROWTH BIAS CORRECTION FACTORS
  ! ======================================================================
  ! HCOR2(ispc) = bias correction for height growth predictions

  real :: HCOR2(MAXSP) = 1.0  ! TODO: Replace with species-specific values

  ! ======================================================================
  ! RECRUITMENT (REGENERATION) CORRECTION FACTORS
  ! ======================================================================
  ! RCOR2(ispc) = bias correction for recruitment/regeneration predictions

  real :: RCOR2(MAXSP) = 1.0  ! TODO: Replace with species-specific values

  ! ======================================================================
  ! SPECIES METADATA
  ! ======================================================================
  ! This section documents the mapping between array indices (1-MAXSP)
  ! and FIA species codes. Maintain this list for reference and documentation.
  !
  ! INDEX | FIA CODE | COMMON NAME              | SCIENTIFIC NAME
  ! ======================================================================
  ! TODO: Fill in your variant's species list
  ! Example for Northeastern variant:
  ! 1     | 12       | Balsam fir               | Abies balsamea
  ! 2     | 71       | Tamarack                 | Larix laricina
  ! 3     | 093      | White spruce             | Picea glauca
  ! etc.
  !
  ! Sources:
  ! * FIA Code Reference: USDA Forest Service
  ! * https://www.fia.fs.fed.us/
  ! * Regional inventory documentation
  !

  ! ======================================================================
  ! SPECIES GROUPING (if used)
  ! ======================================================================
  ! Some models group species for computational efficiency
  ! For example: softwoods vs. hardwoods, or shade-tolerance groups

  ! Example: Which species are softwoods?
  ! SOFTWOOD_MASK(1:MAXSP) = .true. for conifers, .false. for hardwoods

  ! Example: Which species are commercial (merchantable)?
  ! COMMERCIAL(1:MAXSP) = .true. for timber species, .false. otherwise

contains

  subroutine initialize_species_coefficients()
    ! ======================================================================
    ! Load all species-specific coefficients
    ! Called once at program startup before any growth calculations
    !
    ! TODO: This subroutine should:
    ! 1. Read or initialize all species coefficient arrays
    ! 2. Validate that coefficients are within reasonable ranges
    ! 3. Print summary statistics for QA/QC
    ! 4. Set up any derived quantities needed later
    ! ======================================================================

    ! Currently uses MODULE-level initialization via DATA statements
    ! (or PARAMETER declarations for constants)

    ! TODO: Add validation checks
    ! Example:
    ! do ispc = 1, MAXSP
    !   if (BKRAT(ispc) < 0.80 .or. BKRAT(ispc) > 0.99) then
    !     print *, 'Warning: BKRAT out of range for species', ispc
    !   end if
    ! end do

  end subroutine initialize_species_coefficients

  subroutine print_species_summary()
    ! ======================================================================
    ! Print summary information about loaded species
    ! Useful for debugging and documentation
    ! ======================================================================
    integer :: ispc

    print *, '======================================'
    print *, 'FVS Variant Species Configuration'
    print *, '======================================'
    print *, 'Total species configured: ', MAXSP
    print *, ''
    print *, 'Species    Bark Ratio    Corr Factors'
    print *, '---    ----------    ----   ----  ----'

    do ispc = 1, min(20, MAXSP)
      print '(I3, A, F8.4, A, F6.3, A, F6.3, A, F6.3)', &
        ispc, ' |', BKRAT(ispc), ' |', COR2(ispc), &
        ' | ', HCOR2(ispc), ' | ', RCOR2(ispc)
    end do

    if (MAXSP > 20) then
      print *, '... (', MAXSP - 20, ' more species not shown) ...'
    end if

    print *, ''
    print *, 'Average bark ratio:', sum(BKRAT) / real(MAXSP)
    print *, 'Min/Max COR2:', minval(COR2), ' / ', maxval(COR2)

  end subroutine print_species_summary

end module block_data_init
