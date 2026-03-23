# COMMON Block to Fortran Module Migration Guide

## Table of Contents

1. [Current State Analysis](#current-state-analysis)
2. [Migration Strategy](#migration-strategy)
3. [Before/After Examples](#beforeafter-examples)
4. [Step-by-Step Migration Procedure](#step-by-step-migration-procedure)
5. [Handling Variant-Specific Blocks](#handling-variant-specific-blocks)
6. [Testing Strategy](#testing-strategy)
7. [Timeline Estimate](#timeline-estimate)

---

## Current State Analysis

### Key COMMON Block Files Summary

| File | Lines | Type | Primary Content |
|------|-------|------|-----------------|
| ARRAYS.F77 | 263 | Core | Individual tree attributes (DBH, HT, growth, biomass, etc.) |
| CONTRL.F77 | 348 | Core | Control parameters (growth models, output options, thresholds) |
| PLOT.F77 | 191 | Core | Stand/plot-level attributes (BA, TPA, SDI, site info) |
| COEFFS.F77 | 127 | Core | Model coefficients (species-dependent parameters) |

### COMMON Block Structure

#### ARRAYS.F77
Contains per-tree arrays, subscripted by tree index (MAXTRE dimension):
- Logical arrays: LBIRTH (birth status)
- Integer arrays: DAMSEV, DEFECT, ICR, IDTREE, IMC, IND, IND1, IND2, ISP, ISPECL, ITRE, ITRUNC, KUTKOD, NORMHT, DECAYCD, WDLDSTEM
- Real arrays: ABIRTH, BFV, CFV, CRWDTH, DBH, DG, HT, HT2TD, HTG, OLDPCT, OLDRN, PCT, PLTSIZ, PROB, WK1-WK15, YRDLOS, ZRAND, MCFV, SCFV, CULL, ABVGRD_BIO, MERCH_BIO, CUBSAW_BIO, ABVGRD_CARB, MERCH_CARB, CUBSAW_CARB, FOLI_BIO, FOLI_CARB, CARB_FRAC

**Size estimate:** ~80-100 KB (based on MAXTRE dimension)

#### CONTRL.F77
Contains global control parameters and flags, mostly scalars and small arrays:
- Character strings: CFCTYPE, BFCTYPE, VARACD, CALCSDI, TREFMT, KWDFIL, IUSED, NAMGRP, PTGNAME
- Logical flags: LAUTON, LBKDEN, LBVOLS, LCVOLS, LDCOR2, LDUBDG, LFIA, LFIRE, LFLAG, LMORT, LPERM, LRCOR2, LSITE, LSTART, LSUMRY, LTRIP, LZEIDE, MORDAT, NOTRIP, LFIANVB, LDGCAL, LEAVESP, LHTDRG
- Integer parameters: ICCODE, ICFLAG, ICL1-ICL6, ICYC, IDG, IFST, IREAD, IREC1, IREC2, IRECNT, IRECRD, ISTDAT, ITHNPA, ITHNPI, ITHNPN, ITRN, JOCALB, JOLIST, JOSTND, JOSUM, JOTREE, LSTKNT, NCYC, NPTGRP, NSPGRP, NSTKNT, NUMSP
- Real parameters: AUTEFF, AUTMAX, AUTMIN, BAMAX, BAMIN, BFMIN, CCCOEF, CCCOEF2, CFMIN, DBHSDI, DBHSTAGE, DBHZEIDE, DGSD, DR016, EFF, FINTM, PBAWT, PCCFWT, PTPAWT, SCFMIN, SPCLWT, TCFMIN, TCWT, TRM, YR
- Arrays subscripted by species (MAXSP): DBHMIN, FRMCLS, RCOR2, SIZCAP, STMP, TOPD, SCFMIND, SCFTOPD, SCFSTMP, IBEGIN, INS, KOUNT, KPTR, METHB, METHC
- Complex structures: IPTGRP (point groups, 30x52), ISPGRP (species groups, 30x92), ISCT (species indices, MAXSPx2), ITABLE (7 elements)

**Size estimate:** ~20-30 KB

#### PLOT.F77
Contains stand/plot-level attributes (usually single values, not per-tree):
- Character strings: MGMID, ECOREG, CPVREF, NPLT, DBCN, FIAJSP, JSP, NSP, PLNJSP
- Integer scalars: IAGE, IASPEC, ICAGE, ICNTY, IFINT, IFOR, IFORTP, IGL, IIFORTP, IMODTY, IPHREG, IPTINV, ISISP, ISLOP, ISMALL, ISTATE, ISTCL, ISZCL, ITYPE, JSPINDEF, KODFOR, KODTYP, MANAGD, NONSTK, NSITET, ISTDORG
- Integer arrays: IPVEC (MAXPLT), JSPIN (MAXSP), JTYPE (122)
- Real scalars: ASPECT, ATAVD, ATAVH, ATBA, ATCCF, ATSDIX, ATTPA, AVH, BA, BAF, BRK, BTSDIX, COVMLT, COVYR, ELEV, FINT, FPA, GROSPC, OLDAVH, OLDBA, OLDTPA, ORMSQD, PI, PMSDIL, PMSDIU, RELDEN, RELDM1, RMAI, RMSQD, SAMWT, SDIAC, SDIAC2, SDIBC, SDIBC2, SDIMAX, SLOPE, STNDSI, TFPA, TLAT, TLONG, TPROB, VMLT, VMLTYR
- Real arrays: BARANK (MAXSP), RELDSP (MAXSP), SDIDEF (MAXSP), SITEAR (MAXSP), SITETR (MAXSTRx6)

**Size estimate:** ~15-20 KB

#### COEFFS.F77
Contains model coefficients, mostly subscripted by species (MAXSP):
- Integer: IORDER (MAXSP)
- Real scalars: AHAT, BHAT, BJPHI, BJTHET, H2COF, HDGCOF
- Real arrays: ATTEN, BFDEFT (9xMAXSP), BFLA0, BFLA1, BJRHO (40), BKRAT, CFDEFT (9xMAXSP), CFLA0, CFLA1, COR, COR2, CRCON, DGCCF, DGCON, DGDSQ, DIFH, FL, FM, FU, HT1, HT2, HTCON, RHCON, SIGMA, SIGMAR, SMCON, VARDG, WCI

**Size estimate:** ~10-15 KB

### Overall Footprint

- **Total COMMON blocks in codebase:** 104 unique blocks
- **Total source files with INCLUDE statements:** 1,806 files
- **Most heavily used blocks:** ARRAYS, CONTRL, PLOT, COEFFS (and variant-specific ESPARM, PRGPRM)

### COMMON Block Files in Codebase

**Global (root /common directory):** 64 blocks including ARRAYS, CONTRL, PLOT, COEFFS, CALCOM, CALDEN, CICOM, CLIMATE, CVCOM, CWDCOM, DBSTK, ECNCOM, ECNCOMSAVES, ECON, EMCOM, ESCOM2, ESCOMN, ESHAP, ESHAP2, ESHOOT, ESRNCM, ESTCOR, ESTREE, ESWSBW, FVSSTDCM, GGCOM, GLBLCNTL, HTCAL, HVDNCM, INCLUDESVN, KEYCOM, KOTCOM, METRIC, MISCOM, MULTCM, OPCOM, ORGANON, OUTCOM, PDEN, RANCOM, SCREEN, SNCOM, SSTGMC, STDSTK, SUMTAB, SVDATA, SVDEAD, SVRCOM, TWIGCOM, VARCOM, VOLSTD, WORKCM

**Variant-specific (per variant subdirectory):** ESPARM.F77 and PRGPRM.F77 for each of ~20 variants (acd, ak, bm, ca, ci, cr, cs, ec, em, ie, kt, ls, nc, ne, oc, op, pn, sn, so, tt, ut, wc, ws)

---

## Migration Strategy

### Phase 1: Foundation (Weeks 1-4)
Migrate least-dependent core COMMON blocks that are relatively simple and don't have circular dependencies.

**Recommended order:**
1. **COEFFS.F77** (127 lines, ~40 users) - Pure coefficient storage, no dependencies
2. **PLOT.F77** (191 lines, ~80 users) - Stand-level attributes, minimal dependencies
3. **ARRAYS.F77** (263 lines, ~200+ users) - Tree-level arrays, high visibility but self-contained

### Phase 2: Control Parameters (Weeks 5-8)
Migrate control and support blocks.

**Recommended order:**
4. **CONTRL.F77** (348 lines, ~150 users) - Many dependencies on Phase 1 modules
5. Supporting blocks (CALCOM, WORKCM, VARCOM, etc.)

### Phase 3: Specialized/Variant Blocks (Weeks 9+)
Migrate habitat-specific and variant-specific blocks.

**Approach:**
- Create a single variant_commons module per variant that includes variant-specific blocks
- Standardize the interface across all variants

### Key Principles

1. **Maintain compatibility:** Keep old COMMON blocks during transition; use both old and new simultaneously
2. **Single responsibility:** Each module = one logical data domain
3. **Encapsulation:** Make arrays PRIVATE by default, expose only through interfaces
4. **Naming consistency:** Use module names that reflect their purpose (e.g., tree_attributes, stand_parameters)
5. **Access control:** Create getter/setter subroutines for frequently-accessed variables

---

## Before/After Examples

### Example 1: COEFFS.F77 Migration

#### Before (COMMON Block Approach)

```fortran
! In COEFFS.F77
      REAL    AHAT,BHAT,BJPHI,BJTHET,H2COF,HDGCOF
      REAL    ATTEN(MAXSP),BFDEFT(9,MAXSP),BFLA0(MAXSP),BFLA1(MAXSP),
     &        BJRHO(40),BKRAT(MAXSP),CFDEFT(9,MAXSP),CFLA0(MAXSP),
     &        CFLA1(MAXSP),COR(MAXSP),COR2(MAXSP),CRCON(MAXSP),
     &        DGCCF(MAXSP),DGCON(MAXSP),DGDSQ(MAXSP),DIFH(MAXSP),
     &        FL(MAXSP),FM(MAXSP),FU(MAXSP),HT1(MAXSP),HT2(MAXSP),
     &        HTCON(MAXSP),RHCON(MAXSP),SIGMA(MAXSP),SIGMAR(MAXSP),
     &        SMCON(MAXSP),VARDG(MAXSP),WCI(MAXSP)

      COMMON /COEFFS/ AHAT,ATTEN,BFDEFT,BFLA0,BFLA1,BHAT,BJPHI,BJRHO,
     &                BJTHET,BKRAT,CFDEFT,CFLA0,CFLA1,COR,COR2,CRCON,
     &                DGCCF,DGCON,DGDSQ,DIFH,FL,FM,FU,H2COF,HDGCOF,
     &                HT1,HT2,HTCON,IORDER,RHCON,SIGMA,SIGMAR,SMCON,
     &                VARDG,WCI

! In calling code
      INCLUDE 'COEFFS.F77'

      SUBROUTINE DGDRIV(...)
          ! Access coefficients directly
          DO ISP = 1, NUMSP
              SIGMA(ISP) = SQRT(SUM(...))
              COR(ISP) = ...
          END DO
      END SUBROUTINE
```

#### After (Module Approach)

```fortran
! In coeffs_module.f90
MODULE coeffs_module
    USE fvs_parameters, ONLY: MAXSP
    IMPLICIT NONE
    PRIVATE

    ! Public interface
    PUBLIC :: coef_dgccf, coef_sigma, coef_cor, coef_cor2
    PUBLIC :: init_coefficients
    PUBLIC :: set_sigma, get_sigma_by_species

    ! Private module data
    REAL, ALLOCATABLE, TARGET :: sigma(:)
    REAL, ALLOCATABLE, TARGET :: cor(:), cor2(:)
    REAL, ALLOCATABLE, TARGET :: dgccf(:), dgcon(:), dgdsq(:)
    ! ... other coefficients ...

CONTAINS

    SUBROUTINE init_coefficients(maxspecies)
        INTEGER, INTENT(IN) :: maxspecies
        ALLOCATE(sigma(maxspecies))
        ALLOCATE(cor(maxspecies), cor2(maxspecies))
        ALLOCATE(dgccf(maxspecies), dgcon(maxspecies), dgdsq(maxspecies))
        ! ... initialize other arrays ...
    END SUBROUTINE init_coefficients

    SUBROUTINE set_sigma(isp, value)
        INTEGER, INTENT(IN) :: isp
        REAL, INTENT(IN) :: value
        IF (ALLOCATED(sigma)) THEN
            sigma(isp) = value
        END IF
    END SUBROUTINE set_sigma

    FUNCTION get_sigma_by_species(isp) RESULT(val)
        INTEGER, INTENT(IN) :: isp
        REAL :: val
        IF (ALLOCATED(sigma)) THEN
            val = sigma(isp)
        ELSE
            val = 0.0
        END IF
    END FUNCTION get_sigma_by_species

END MODULE coeffs_module

! In calling code
    USE coeffs_module

    SUBROUTINE dgdriv(...)
        ! Access coefficients through subroutine interface
        DO isp = 1, numsp
            CALL set_sigma(isp, SQRT(SUM(...)))
            ! ... other operations ...
        END DO
    END SUBROUTINE
```

### Example 2: PLOT.F77 Migration (Partial)

#### Before (COMMON Block)

```fortran
! In PLOT.F77
      CHARACTER*4 MGMID, ECOREG
      CHARACTER*10 CPVREF
      CHARACTER*26 NPLT
      CHARACTER*40 DBCN

      INTEGER IAGE,IASPEC,ICAGE,ICNTY,IFINT,IFOR,IFORTP,IGL

      REAL BA,BAF,BRK,ELEV,FINT,FPA,ASPECT,SLOPE,STNDSI

      REAL BARANK(MAXSP),RELDSP(MAXSP),SDIDEF(MAXSP),SITEAR(MAXSP)
      REAL SITETR(MAXSTR,6)

      COMMON /PLTCHR/ CPVREF,DBCN,ECOREG,FIAJSP,JSP,MGMID,NPLT,NSP,
     &                PLNJSP

      COMMON /PLOT/ ASPECT,ATAVD,ATAVH,ATBA,ATCCF,ATSDIX,ATTPA,AVH,BA,
     &              BAF,BARANK,BRK,BTSDIX,COVMLT,COVYR,ELEV,FINT,FPA,
     &              ...

! In calling code
      INCLUDE 'PLOT.F77'
      INCLUDE 'COEFFS.F77'

      SUBROUTINE STKVAL(...)
          ! Stand is stocking class: find BA and TPA
          ISTCL = 1 + INT(BA/10.0)
          IF (BA < 50.0) THEN
              ISZCL = 1
          END IF
      END SUBROUTINE
```

#### After (Module)

```fortran
! In plot_module.f90
MODULE plot_module
    USE fvs_parameters, ONLY: MAXSP, MAXSTR, MAXPLT
    USE coeffs_module, ONLY: get_sdidef_by_species
    IMPLICIT NONE
    PRIVATE

    ! Public interface
    PUBLIC :: init_plot_data
    PUBLIC :: get_plot_ba, set_plot_ba
    PUBLIC :: get_plot_tpa, set_plot_tpa
    PUBLIC :: get_plot_age, set_plot_age
    PUBLIC :: plot_info_type

    ! Type to encapsulate plot data
    TYPE :: plot_info_type
        CHARACTER(LEN=26) :: stand_id
        CHARACTER(LEN=4) :: mgmt_id
        CHARACTER(LEN=4) :: ecoreg
        INTEGER :: age, init_age, state_code, county_code
        REAL :: elevation, aspect, slope, site_index
        REAL :: basal_area, tpa, quadratic_mean_dbh
        REAL :: crown_competition_factor
        REAL :: sdi_before_cut, sdi_after_cut
        INTEGER :: stocking_class, size_class
    END TYPE plot_info_type

    TYPE(plot_info_type), TARGET :: plot_data

CONTAINS

    SUBROUTINE init_plot_data()
        plot_data%stand_id = ''
        plot_data%basal_area = 0.0
        plot_data%tpa = 0.0
        plot_data%age = 0
        ! ... initialize other fields ...
    END SUBROUTINE init_plot_data

    FUNCTION get_plot_ba() RESULT(ba)
        REAL :: ba
        ba = plot_data%basal_area
    END FUNCTION get_plot_ba

    SUBROUTINE set_plot_ba(value)
        REAL, INTENT(IN) :: value
        plot_data%basal_area = value
    END SUBROUTINE set_plot_ba

    ! Similar getter/setter for TPA, age, etc.

    FUNCTION calculate_stocking_class(ba) RESULT(istcl)
        REAL, INTENT(IN) :: ba
        INTEGER :: istcl
        istcl = 1 + INT(ba / 10.0)
        IF (ba < 50.0) istcl = 1
    END FUNCTION calculate_stocking_class

END MODULE plot_module

! In calling code
    USE plot_module

    SUBROUTINE stkval(...)
        REAL :: ba
        INTEGER :: istcl

        ba = get_plot_ba()
        istcl = calculate_stocking_class(ba)
        ! ... rest of logic ...
    END SUBROUTINE
```

### Example 3: ARRAYS.F77 Migration (Partial)

#### Before (COMMON Block)

```fortran
! In ARRAYS.F77
      LOGICAL LBIRTH(MAXTRE)

      INTEGER DAMSEV(6,MAXTRE),DEFECT(MAXTRE),ICR(MAXTRE),
     &        IDTREE(MAXTRE),IMC(MAXTRE),IND(MAXTRE),IND1(MAXTRE),
     &        IND2(MAXTRE),ISP(MAXTRE),ISPECL(MAXTRE)

      REAL DBH(MAXTRE),DG(MAXTRE),HT(MAXTRE),HT2TD(MAXTRE,2),
     &     HTG(MAXTRE),OLDPCT(MAXTRE),PCT(MAXTRE),PROB(MAXTRE),
     &     CFV(MAXTRE),BFV(MAXTRE),CRWDTH(MAXTRE)

      COMMON /ARRAYS/ ABIRTH,BFV,CFV,CRWDTH,DAMSEV,DBH,DEFECT,DG,HT,
     &                HT2TD,HTG,ICR,IDTREE,IMC,IND,IND1,IND2,ISP,
     &                ... (very long list)

! In calling code
      INCLUDE 'ARRAYS.F77'

      SUBROUTINE VOLS(I)
          ! Access tree arrays with index I
          DBH(I) = DBH(I) + DG(I)
          CFV(I) = CALCULATE_CFV(DBH(I), HT(I), ISP(I))
          BFV(I) = CALCULATE_BFV(DBH(I), HT(I), ISP(I))
      END SUBROUTINE
```

#### After (Derived Type with Module)

```fortran
! In tree_module.f90
MODULE tree_module
    USE fvs_parameters, ONLY: MAXTRE
    IMPLICIT NONE
    PRIVATE

    ! Encapsulate all tree attributes in a derived type
    TYPE :: tree_record
        ! Tree identification
        INTEGER :: id_tree
        INTEGER :: species_code
        INTEGER :: special_status_code

        ! Physical attributes
        REAL :: dbh                    ! Diameter at breast height
        REAL :: height                 ! Total height
        REAL :: crown_width
        REAL :: crown_ratio
        INTEGER :: crown_ratio_percent

        ! Growth attributes
        REAL :: dbh_increment_periodic
        REAL :: height_increment

        ! Volume attributes
        REAL :: cuft_volume            ! Cubic foot volume
        REAL :: bfv                    ! Board foot volume
        REAL :: merch_cubic_volume
        REAL :: sawlog_cubic_volume
        REAL :: percent_cull

        ! Mortality and damage
        LOGICAL :: born_in_period
        INTEGER :: damage_codes(6)
        INTEGER :: decay_code
        INTEGER :: truncation_height

        ! Density and distribution
        REAL :: trees_per_acre
        REAL :: percentile_ba_dist
        REAL :: basal_area_random_error

        ! Working arrays
        REAL :: work1, work2, work3, work4
        REAL :: work5, work6, work7, work8
        REAL :: work9, work10, work11, work12
        REAL :: work13, work14, work15

        ! Biomass and carbon
        REAL :: above_ground_biomass
        REAL :: merch_biomass
        REAL :: cubsaw_biomass
        REAL :: foliage_biomass
        REAL :: above_ground_carbon
        REAL :: merch_carbon
        REAL :: cubsaw_carbon
        REAL :: foliage_carbon
        REAL :: carbon_fraction
    END TYPE tree_record

    ! Array of tree records
    TYPE(tree_record), ALLOCATABLE :: trees(:)
    INTEGER :: ntrees

    ! Public interface
    PUBLIC :: tree_record, trees, ntrees
    PUBLIC :: init_tree_arrays
    PUBLIC :: get_tree, set_tree_dbh, set_tree_volume

CONTAINS

    SUBROUTINE init_tree_arrays(max_trees)
        INTEGER, INTENT(IN) :: max_trees
        ALLOCATE(trees(max_trees))
        ntrees = 0
    END SUBROUTINE init_tree_arrays

    FUNCTION get_tree(i) RESULT(tree)
        INTEGER, INTENT(IN) :: i
        TYPE(tree_record) :: tree
        IF (i >= 1 .AND. i <= ntrees) THEN
            tree = trees(i)
        END IF
    END FUNCTION get_tree

    SUBROUTINE set_tree_dbh(i, dbh_val)
        INTEGER, INTENT(IN) :: i
        REAL, INTENT(IN) :: dbh_val
        IF (i >= 1 .AND. i <= ntrees) THEN
            trees(i)%dbh = dbh_val
        END IF
    END SUBROUTINE set_tree_dbh

    SUBROUTINE set_tree_volume(i, cf_vol, bf_vol)
        INTEGER, INTENT(IN) :: i
        REAL, INTENT(IN) :: cf_vol, bf_vol
        IF (i >= 1 .AND. i <= ntrees) THEN
            trees(i)%cuft_volume = cf_vol
            trees(i)%bfv = bf_vol
        END IF
    END SUBROUTINE set_tree_volume

END MODULE tree_module

! In calling code
    USE tree_module

    SUBROUTINE vols(i)
        TYPE(tree_record) :: tree

        tree = get_tree(i)
        tree%dbh = tree%dbh + tree%dbh_increment_periodic
        tree%cuft_volume = calculate_cfv(tree%dbh, tree%height, tree%species_code)
        tree%bfv = calculate_bfv(tree%dbh, tree%height, tree%species_code)

        CALL set_tree_dbh(i, tree%dbh)
        CALL set_tree_volume(i, tree%cuft_volume, tree%bfv)
    END SUBROUTINE
```

---

## Step-by-Step Migration Procedure

### Complete Migration Example: COEFFS.F77

#### Step 1: Create Module File

Create `/path/to/fvs/fvs_coefficients.f90`:

```fortran
MODULE fvs_coefficients
    USE fvs_parameters, ONLY: MAXSP
    IMPLICIT NONE
    SAVE

    ! Coefficient arrays (subscripted by species)
    INTEGER, ALLOCATABLE :: coef_iorder(:)
    REAL, ALLOCATABLE :: coef_ahat, coef_bhat
    REAL, ALLOCATABLE :: coef_atten(:)
    REAL, ALLOCATABLE :: coef_bfdeft(:,:)
    REAL, ALLOCATABLE :: coef_bfla0(:), coef_bfla1(:)
    REAL, ALLOCATABLE :: coef_bjphi, coef_bjthet
    REAL, ALLOCATABLE :: coef_bjrho(:)
    REAL, ALLOCATABLE :: coef_bkrat(:)
    REAL, ALLOCATABLE :: coef_cfdeft(:,:)
    REAL, ALLOCATABLE :: coef_cfla0(:), coef_cfla1(:)
    REAL, ALLOCATABLE :: coef_cor(:), coef_cor2(:)
    REAL, ALLOCATABLE :: coef_crcon(:)
    REAL, ALLOCATABLE :: coef_dgccf(:), coef_dgcon(:), coef_dgdsq(:)
    REAL, ALLOCATABLE :: coef_difh(:)
    REAL, ALLOCATABLE :: coef_fl(:), coef_fm(:), coef_fu(:)
    REAL, ALLOCATABLE :: coef_h2cof, coef_hdgcof
    REAL, ALLOCATABLE :: coef_ht1(:), coef_ht2(:)
    REAL, ALLOCATABLE :: coef_htcon(:)
    REAL, ALLOCATABLE :: coef_rhcon(:)
    REAL, ALLOCATABLE :: coef_sigma(:), coef_sigmar(:)
    REAL, ALLOCATABLE :: coef_smcon(:)
    REAL, ALLOCATABLE :: coef_vardg(:)
    REAL, ALLOCATABLE :: coef_wci(:)

CONTAINS

    SUBROUTINE initialize_coefficients(nspecies)
        INTEGER, INTENT(IN) :: nspecies

        ALLOCATE(coef_iorder(nspecies))
        ALLOCATE(coef_atten(nspecies))
        ALLOCATE(coef_bfdeft(9,nspecies))
        ALLOCATE(coef_bfla0(nspecies), coef_bfla1(nspecies))
        ALLOCATE(coef_bjrho(40))
        ALLOCATE(coef_bkrat(nspecies))
        ALLOCATE(coef_cfdeft(9,nspecies))
        ALLOCATE(coef_cfla0(nspecies), coef_cfla1(nspecies))
        ALLOCATE(coef_cor(nspecies), coef_cor2(nspecies))
        ALLOCATE(coef_crcon(nspecies))
        ALLOCATE(coef_dgccf(nspecies), coef_dgcon(nspecies), coef_dgdsq(nspecies))
        ALLOCATE(coef_difh(nspecies))
        ALLOCATE(coef_fl(nspecies), coef_fm(nspecies), coef_fu(nspecies))
        ALLOCATE(coef_ht1(nspecies), coef_ht2(nspecies))
        ALLOCATE(coef_htcon(nspecies))
        ALLOCATE(coef_rhcon(nspecies))
        ALLOCATE(coef_sigma(nspecies), coef_sigmar(nspecies))
        ALLOCATE(coef_smcon(nspecies))
        ALLOCATE(coef_vardg(nspecies))
        ALLOCATE(coef_wci(nspecies))

        ! Initialize to default values
        coef_iorder = 0
        coef_ahat = 0.0
        coef_bhat = 0.0
        coef_atten = 0.0
        coef_bfdeft = 0.0
        ! ... etc for all arrays ...

    END SUBROUTINE initialize_coefficients

    ! Getter/setter for commonly accessed values
    REAL FUNCTION get_sigma(ispecies)
        INTEGER, INTENT(IN) :: ispecies
        IF (ALLOCATED(coef_sigma) .AND. ispecies > 0 .AND. ispecies <= SIZE(coef_sigma)) THEN
            get_sigma = coef_sigma(ispecies)
        ELSE
            get_sigma = 0.0
        END IF
    END FUNCTION get_sigma

    SUBROUTINE set_sigma(ispecies, value)
        INTEGER, INTENT(IN) :: ispecies
        REAL, INTENT(IN) :: value
        IF (ALLOCATED(coef_sigma) .AND. ispecies > 0 .AND. ispecies <= SIZE(coef_sigma)) THEN
            coef_sigma(ispecies) = value
        END IF
    END SUBROUTINE set_sigma

    ! ... similar getters/setters for other frequently-used arrays ...

END MODULE fvs_coefficients
```

#### Step 2: Update Calling Code (with Both Old and New)

```fortran
! Option A: Hybrid approach (keep both for transition)
      INCLUDE 'COEFFS.F77'           ! Old COMMON block for safety
      USE fvs_coefficients           ! New module

      SUBROUTINE DGDRIV(...)
          ! Use new module interface where possible
          CALL set_sigma(isp, sqrt(sum_value))

          ! Fall back to old COMMON for complex operations
          DO J = 1, 40
              SIGMA(ISP) = SIGMA(ISP) * BJRHO(J)
          END DO
      END SUBROUTINE
```

#### Step 3: Gradually Replace COMMON Block References

**Before (using COMMON):**
```fortran
      INCLUDE 'COEFFS.F77'
      SIGMA(ISP) = SQRT(SUM(...))
      COR(ISP) = COR(ISP) * WGHT
```

**After (using module):**
```fortran
      USE fvs_coefficients
      CALL set_sigma(isp, SQRT(SUM(...)))
      CALL scale_cor(isp, wght)
```

#### Step 4: Compile Both (Old and New)

During transition, the compiler will see both:
- Old code using INCLUDE 'COEFFS.F77' (COMMON block)
- New code using module

**Compilation sequence:**
```bash
# 1. Compile module first
gfortran -c fvs_coefficients.f90

# 2. Compile other modules that depend on it
gfortran -c fvs_parameters.f90
gfortran -c other_modules.f90

# 3. Compile main program (which includes old COMMON and uses new modules)
gfortran -c main.f
gfortran -c other_source.f

# 4. Link
gfortran -o fvs *.o
```

#### Step 5: Test Equivalence

Create a test module to ensure old COMMON and new module stay synchronized:

```fortran
MODULE coef_transition_tests
    USE fvs_coefficients
    IMPLICIT NONE
CONTAINS

    SUBROUTINE test_coef_equivalence(nsp)
        INTEGER, INTENT(IN) :: nsp
        INTEGER :: isp
        REAL :: old_val, new_val
        REAL, ALLOCATABLE :: old_sigma(:)

        ! Read old COMMON values
        ALLOCATE(old_sigma(nsp))
        ! ... copy from old COMMON block ...

        ! Compare with new module
        DO isp = 1, nsp
            new_val = get_sigma(isp)
            IF (ABS(old_val - new_val) > 1.0E-6) THEN
                PRINT *, 'ERROR: Sigma mismatch at species ', isp
                PRINT *, '  Old (COMMON): ', old_val
                PRINT *, '  New (module): ', new_val
            END IF
        END DO

        DEALLOCATE(old_sigma)
    END SUBROUTINE test_coef_equivalence

END MODULE coef_transition_tests
```

#### Step 6: Decommission COMMON Block

Once all references are updated and tests pass:
1. Remove INCLUDE 'COEFFS.F77' statements
2. Delete the COEFFS.F77 file
3. Update build system to remove COEFFS.F77 dependency
4. Final full compilation and testing

---

## Handling Variant-Specific Blocks

### Problem
Each variant (AK, NC, EM, etc.) has its own ESPARM.F77 and PRGPRM.F77, creating ~46 separate COMMON block files (~20 variants × 2 blocks).

### Solution: Create Variant-Aware Modules

#### Approach 1: Single Module with Variant Selector

```fortran
MODULE variant_parameters
    INTEGER, PARAMETER :: VARIANT_AK = 1
    INTEGER, PARAMETER :: VARIANT_NC = 2
    INTEGER, PARAMETER :: VARIANT_EM = 3
    ! ... etc

    INTEGER, SAVE :: current_variant = 0

    ! Arrays for all variants (could use TYPE to group by variant)
    REAL, ALLOCATABLE :: esp_array1_ak(:), esp_array2_ak(:)
    REAL, ALLOCATABLE :: esp_array1_nc(:), esp_array2_nc(:)
    ! ... etc

CONTAINS

    SUBROUTINE set_variant(var_code)
        INTEGER, INTENT(IN) :: var_code
        current_variant = var_code
    END SUBROUTINE set_variant

    REAL FUNCTION get_esp_param(ispecies, iarray)
        INTEGER, INTENT(IN) :: ispecies, iarray
        SELECT CASE (current_variant)
            CASE (VARIANT_AK)
                SELECT CASE (iarray)
                    CASE (1)
                        get_esp_param = esp_array1_ak(ispecies)
                    CASE (2)
                        get_esp_param = esp_array2_ak(ispecies)
                END SELECT
            CASE (VARIANT_NC)
                ! ... similar for NC variant ...
        END SELECT
    END FUNCTION get_esp_param

END MODULE variant_parameters
```

#### Approach 2: Variant Modules with Common Interface

```fortran
! ak_variant_module.f90
MODULE ak_variant_parameters
    USE fvs_parameters
    IMPLICIT NONE

    ! AK-specific parameters
    REAL, ALLOCATABLE :: ak_esp_param1(:), ak_esp_param2(:)
    ! ... AK-specific arrays ...

CONTAINS
    ! AK implementation
END MODULE ak_variant_parameters

! nc_variant_module.f90
MODULE nc_variant_parameters
    USE fvs_parameters
    IMPLICIT NONE

    ! NC-specific parameters
    REAL, ALLOCATABLE :: nc_esp_param1(:), nc_esp_param2(:)
    ! ... NC-specific arrays ...

CONTAINS
    ! NC implementation
END MODULE nc_variant_parameters

! variant_factory.f90
MODULE variant_factory
CONTAINS

    SUBROUTINE load_variant_parameters(variant_code)
        CHARACTER(LEN=*), INTENT(IN) :: variant_code
        SELECT CASE (TRIM(variant_code))
            CASE ('AK')
                USE ak_variant_parameters
                CALL init_ak_parameters()
            CASE ('NC')
                USE nc_variant_parameters
                CALL init_nc_parameters()
            ! ... etc
        END SELECT
    END SUBROUTINE load_variant_parameters

END MODULE variant_factory
```

### Variant COMMON Block File Locations

**Current structure:**
```
/home/aweiskittel/ForestVegetationSimulator-main/
  ├── acd/common/ESPARM.F77, PRGPRM.F77
  ├── ak/common/ESPARM.F77, PRGPRM.F77
  ├── bm/common/ESPARM.F77, PRGPRM.F77
  ├── ca/common/ESPARM.F77, PRGPRM.F77
  ├── ci/common/ESPARM.F77, PRGPRM.F77
  ├── cr/common/ESPARM.F77, PRGPRM.F77
  ├── cs/common/ESPARM.F77, PRGPRM.F77
  ├── ec/common/ESPARM.F77, PRGPRM.F77
  ├── em/common/ESPARM.F77, PRGPRM.F77
  ├── ie/common/ESPARM.F77, PRGPRM.F77
  ├── kt/common/ESPARM.F77, PRGPRM.F77
  ├── ls/common/ESPARM.F77, PRGPRM.F77
  ├── nc/common/ESPARM.F77, PRGPRM.F77
  ├── ne/common/ESPARM.F77, PRGPRM.F77
  ├── oc/common/ESPARM.F77, PRGPRM.F77
  ├── op/common/ESPARM.F77, PRGPRM.F77
  ├── pn/common/ESPARM.F77, PRGPRM.F77
  ├── sn/common/ESPARM.F77, PRGPRM.F77
  ├── so/common/ESPARM.F77, PRGPRM.F77
  ├── tt/common/ESPARM.F77, PRGPRM.F77
  ├── ut/common/ESPARM.F77, PRGPRM.F77
  ├── wc/common/ESPARM.F77, PRGPRM.F77
  └── ws/common/ESPARM.F77, PRGPRM.F77
```

**Recommended post-migration structure:**
```
  ├── fvs_modules/
  │   ├── fvs_variant_parameters.f90
  │   ├── variants/
  │   │   ├── ak_variant.f90
  │   │   ├── nc_variant.f90
  │   │   ├── em_variant.f90
  │   │   └── ...
  │   └── variant_loader.f90
```

---

## Testing Strategy

### 1. Unit Tests for Each Module

```fortran
! test_coeffs_module.f90
PROGRAM test_coefficients
    USE fvs_coefficients
    IMPLICIT NONE
    INTEGER :: i, nsp
    REAL :: test_val, retrieved_val

    nsp = 25
    CALL initialize_coefficients(nsp)

    ! Test 1: Set and get sigma
    test_val = 3.14159
    CALL set_sigma(1, test_val)
    retrieved_val = get_sigma(1)

    IF (ABS(test_val - retrieved_val) < 1.0E-6) THEN
        PRINT *, 'PASS: Sigma get/set works correctly'
    ELSE
        PRINT *, 'FAIL: Sigma mismatch'
        STOP 1
    END IF

    ! Test 2: Array bounds checking
    retrieved_val = get_sigma(0)  ! Should return 0 safely
    RETRIEVED_VAL = get_sigma(nsp+1)  ! Should return 0 safely

    PRINT *, 'All unit tests passed!'
END PROGRAM test_coefficients
```

### 2. Regression Tests

Compare output (stand summaries, tree lists) between:
- Old code (using COMMON blocks)
- New code (using modules)
- Hybrid code (using both)

```bash
# Run old and new on same input
./fvs_old < test_input.kwd > output_old.txt
./fvs_new < test_input.kwd > output_new.txt

# Compare outputs
diff output_old.txt output_new.txt
```

### 3. Build Validation Tests

Ensure both compilation methods work:
```bash
# 1. Old build (COMMON blocks only)
make clean
make -DUSE_COMMON_BLOCKS
./fvs_old < test.kwd > result_old.txt

# 2. New build (modules only)
make clean
make -DUSE_MODULES_ONLY
./fvs_new < test.kwd > result_new.txt

# 3. Hybrid build (both, modules used first)
make clean
make -DUSE_BOTH
./fvs_hybrid < test.kwd > result_hybrid.txt

# 4. Compare all
diff result_old.txt result_new.txt
diff result_new.txt result_hybrid.txt
```

### 4. Scope and Bounds Tests

```fortran
! Verify that module data is properly scoped
MODULE test_scope
    IMPLICIT NONE
    REAL, PRIVATE :: private_data = 99.0
    REAL, PUBLIC :: public_data = 42.0
CONTAINS
    SUBROUTINE verify_scope()
        ! This should compile and run fine
        PRINT *, 'Private data: ', private_data
        PRINT *, 'Public data: ', public_data
    END SUBROUTINE
END MODULE test_scope
```

---

## Timeline Estimate

### Week 1-2: Planning and Foundation
* Analyze current COMMON block usage (completed)
* Design module structure and interfaces: 3-4 days
* Create build system modifications: 2-3 days
* Create COEFFS module: 1-2 days
* **Deliverables:** Design document, COEFFS module, updated build system

### Week 3-4: Phase 1 Core Modules
* Create PLOT module: 3-4 days
* Create ARRAYS module (complex, largest): 4-5 days
* Create unit tests: 2-3 days
* Initial regression testing: 2-3 days
* **Deliverables:** PLOT and ARRAYS modules, test suite, regression test results

### Week 5-6: Phase 1 Completion
* Create CONTRL module: 4-5 days
* Create supporting modules (VARCOM, WORKCM, KEYCOM): 4-5 days
* Comprehensive testing: 3-4 days
* Documentation updates: 2 days
* **Deliverables:** All Phase 1 modules, comprehensive test suite

### Week 7-8: Phase 2 Variant Modules
* Analyze variant-specific COMMON blocks: 2-3 days
* Create variant module framework: 3-4 days
* Create variant-specific modules (all 20 variants): 6-8 days
* **Deliverables:** Variant module structure, all variant modules

### Week 9-10: Phase 2 Completion & Integration
* Full build and integration testing: 5-6 days
* Performance validation: 2-3 days
* Documentation: 3-4 days
* **Deliverables:** Fully working codebase with modules, final testing report

### Week 11-12: Cleanup & Decommissioning
* Remove COMMON block includes from source: 2-3 days
* Final comprehensive testing: 4-5 days
* Performance optimization (if needed): 2-3 days
* Final documentation and knowledge transfer: 2-3 days
* **Deliverables:** Clean codebase without COMMON blocks, final report

### Estimated Total Effort
* **Calendar time:** 12 weeks (3 months)
* **Full-time equivalent:** ~1.5-2.0 developers (some parallelizable work)
* **With 1 developer:** 15-18 weeks (4-4.5 months)
* **With 2 developers:** 8-10 weeks (2-2.5 months)

### Critical Path
1. COEFFS module → PLOT → ARRAYS → CONTRL (foundational)
2. Variant modules (parallelizable)
3. Integration and testing
4. Decommissioning

---

## Maintenance and Best Practices

### Post-Migration

1. **Module Organization:**
   - Keep modules in dedicated directories
   - One module file per logical domain
   - Clear naming conventions

2. **Documentation:**
   - Update internal comments to explain module purposes
   - Maintain API documentation for public interfaces
   - Document initialization sequence

3. **Version Control:**
   - Tag milestone commits (e.g., "Phase1_Complete", "AllModulesConverted")
   - Maintain separate branches for each phase
   - Use CI/CD to automatically test all variants

4. **Code Review:**
   - All module changes require review
   - Check for COMMON block remnants
   - Verify bounds checking and error handling

### Fallback Plan

If issues arise during migration:
1. Keep COMMON block files available for 1-2 releases
2. Maintain "dual mode" builds (COMMON or modules)
3. Switch back to COMMON blocks if critical issues found
4. Investigate and fix, then re-migrate

---

## Conclusion

The migration from COMMON blocks to Fortran modules is a significant modernization effort that improves:
- **Code maintainability** through better encapsulation
- **Type safety** with derived types
- **Debugging capability** through scoped variables
- **Compiler optimization** potential
- **Future-proofing** for Fortran standardization

The recommended phased approach allows for incremental progress, continuous testing, and minimal disruption to ongoing development.

