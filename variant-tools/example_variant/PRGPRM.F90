! ============================================================================
! TEMPLATE: FVS Program Parameters for Variant
! ============================================================================
!
! This module defines fundamental parameters that control memory allocation
! and array sizes for the Forest Vegetation Simulator model. These parameters
! must be set before compilation.
!
! IMPORTANT: Changing these parameters requires recompilation of the entire
! program. The MAXSP (maximum species) parameter in particular affects the
! size of all species-dimensioned coefficient arrays throughout the model.
!
! ============================================================================

module program_parameters
  implicit none

  ! ====================================================================
  ! TREE AND PLOT PARAMETERS
  ! ====================================================================

  integer, parameter :: MAXTRE = 3000
  ! Maximum number of tree records that the model can process.
  ! Typical inventory plots have 100-300 trees; maximum is 3000.
  ! NOTE: Increasing this significantly increases memory usage.
  ! Realistic minimum for full functionality: ~400 trees.

  integer, parameter :: MAXTP1 = MAXTRE + 1
  ! Maximum number of tree records plus one (used for array bounds).
  ! Automatically computed from MAXTRE.

  integer, parameter :: MAXPLT = 500
  ! Maximum number of individual plots (subplots) that the model
  ! can process in a single run.

  ! ====================================================================
  ! SPECIES PARAMETER
  ! ====================================================================

  integer, parameter :: MAXSP = 108
  ! Maximum number of species that the model recognizes.
  ! For FVS, this corresponds to FIA (Forest Inventory and Analysis)
  ! species code coverage. The standard is 108 species to cover the
  ! entire United States, but regional variants may use fewer.
  !
  ! TODO: If your variant is for a specific region, you might reduce
  ! this to the number of actual species present (e.g., 60 for a
  ! southeastern variant, 80 for a northeastern variant).
  ! However, keeping MAXSP=108 for national compatibility is recommended.
  !
  ! This parameter controls the size of all species-dimensioned arrays:
  ! * Crown ratio coefficients (BCR1-BCR4, each MAXSP long)
  ! * Diameter growth coefficients (B1, B2, etc.)
  ! * Mortality/survival parameters
  ! * Height-diameter relationships
  ! * Bark ratios (BKRAT)
  ! * And many other coefficient arrays
  !
  ! Total species coefficient memory ≈ (# of coefficients per species) * MAXSP * 8 bytes
  ! With ~50 coefficient arrays, this is roughly 50 * 108 * 8 = 43 KB per variant

  ! ====================================================================
  ! SIMULATION CYCLE PARAMETERS
  ! ====================================================================

  integer, parameter :: MAXCYC = 40
  ! Maximum number of growth cycles allowed in a single simulation run.
  ! Each cycle typically represents 5 years of growth.
  ! MAXCYC = 40 allows projections up to 200 years.

  integer, parameter :: MAXCY1 = MAXCYC + 1
  ! Maximum cycles plus one (used for array indexing).
  ! Automatically computed from MAXCYC.

  ! ====================================================================
  ! MISCELLANEOUS PARAMETERS
  ! ====================================================================

  integer, parameter :: MAXSTR = 20
  ! Maximum number of site trees (representative trees used for
  ! stand-level statistics and growth curves).

  integer, parameter :: MXFRCDS = 20
  ! Maximum number of forest codes / type classes.
  ! Used for tracking stand origin and management history.

  ! ====================================================================
  ! PARAMETER RELATIONSHIPS AND NOTES
  ! ====================================================================
  ! The following relationships should always hold:
  !
  ! 1. MAXSP >= number of species in your variant
  !    (For national compatibility, use MAXSP = 108)
  !
  ! 2. MAXCYC >= (maximum projection years / cycle length)
  !    If cycles are 5 years and max projection is 200 years:
  !    MAXCYC >= 200/5 = 40
  !
  ! 3. MAXTRE >= maximum trees on a plot
  !    Typical large plots have 200-300 trees
  !    Clusters with multiple plots might have 500-1000 trees
  !
  ! 4. MAXPLT >= number of plots to process simultaneously
  !
  ! Memory Usage Estimate:
  ! * Each tree record: ~200 bytes of arrays
  ! * Total tree storage: MAXTRE * 200 bytes = 3000 * 200 = 600 KB
  ! * Coefficient arrays: ~43 KB (as calculated above)
  ! * Other common blocks: ~100 KB
  ! * Total: ~750 KB base + 200 bytes per active tree record
  !
  ! For a typical run with 2000 trees on 5 plots: ~400 KB + overhead

  ! ====================================================================
  ! VARIANT-SPECIFIC CUSTOMIZATION SUGGESTIONS
  ! ====================================================================
  ! If optimizing for your specific variant:
  !
  ! 1. Set MAXSP to actual species count + 5 buffer
  !    E.g., if your region has 85 species, use MAXSP = 90
  !    This reduces memory and array initialization time
  !
  ! 2. Set MAXTRE based on regional inventory standards
  !    E.g., if plots typically have max 250 trees, use MAXTRE = 300
  !
  ! 3. Set MAXCYC based on typical projection length
  !    E.g., if most projections are 100 years (20 cycles), MAXCYC = 25
  !
  ! WARNING: Any changes to these parameters require:
  ! 1. Updating all coefficient array declarations in BLKDAT
  ! 2. Recompiling the entire program
  ! 3. Re-testing with sample inventories to verify memory usage
  ! 4. Documenting the changes in variant release notes

end module program_parameters
