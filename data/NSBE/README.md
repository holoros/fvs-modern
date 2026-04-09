# NSBE/VTECO Volume and Biomass Equations

This directory contains coefficient files for the Westfall et al. (2024) NSBE (Northeast State-based Equations) system, adapted with VTECO coefficients for species × ecodivision specificity.

## Reference

**Westfall, J. A., et al. (2024)**. Northeast State-based Volume and Biomass Equations: Documentation and Application Guide. USDA Forest Service, Northern Research Station.

## Directory Structure

```
NSBE/
├── README.md                           # This file
├── REF_SPECIES.csv                     # Species reference table (SPCD lookup)
├── Coefs/combined/                     # Primary coefficient directory
│   ├── volib_coefs.csv                # Volume inside-bark by SPCD × ecodivision
│   ├── volob_coefs.csv                # Volume outside-bark
│   ├── volbk_coefs.csv                # Bark volume (supplementary)
│   ├── total_biomass_coefs.csv         # Total aboveground biomass
│   ├── bark_biomass_coefs.csv          # Bark component biomass
│   ├── branch_biomass_coefs.csv        # Branch component biomass
│   ├── foliage_coefs.csv               # Foliage biomass (optional)
│   ├── rcumib_coefs.csv                # Merchantable volume inside-bark (optional)
│   └── rcumob_coefs.csv                # Merchantable volume outside-bark (optional)
└── Files/
    └── equation_forms_and_calls.csv    # Equation form definitions
```

## Key Files

### REF_SPECIES.csv
Species reference table with:
- `SPCD`: FVS species code
- `GENUS`, `SPECIES`: Taxonomic classification
- `JENKINS_SPGRPCD`: Jenkins species group (fallback for missing coefficients)
- `WOOD_SPGR_GREENVOL_DRYWT`: Green volume/dry weight specific gravity
- `BARK_SPGR_GREENVOL_DRYWT`: Bark-specific gravity

### Coefficient Tables
Each coefficient file contains:
- `SPCD_DIVISION`: Species × ecodivision code (e.g., "12 M330")
- `SPCD`: FVS species code (fallback level 2)
- `JENKINS_SPGRPCD`: Jenkins group code (fallback level 3)
- `equation`: Form number (3, 4, 5, 50, etc.)
- Coefficient columns: `a`, `b`, `c`, `b2`, `a0`, `b0`, `b1`, `a1`, `c1`

### Coefficient Hierarchy
For any species, coefficients are selected in this priority:

1. **SPCD_DIVISION** (most specific): Species × ecodivision combination
   - e.g., "12 M330" = Balsam fir in mixed conifer forest ecodivision
2. **SPCD** (intermediate): Species alone, ignoring ecodivision
3. **JENKINS_SPGRPCD** (least specific): Jenkins species group
   - Used when no SPCD-level coefficients exist

## Equation Forms

| Form | Equation | Use |
|------|----------|-----|
| 3    | `V = a × DBH^b × HT^c` | Standard allometric (most common) |
| 4    | `V = ifelse(DBH < k, a0×DBH^b0×HT^c, a0×k^(b0-b1)×DBH^b1×HT^c)` | Segmented DBH (two-part) |
| 5    | `V = a × DBH^(a1×(1-exp(-b1×DBH))^c1) × HT^c` | Variable exponent |
| 50   | `V = a × DBH^b × HT^c × exp(-(b2×DBH))` | Exponential decay |

### Input Units
- **DBH**: inches (FVS/FIA standard)
- **HT**: feet (FVS/FIA standard)

### Output Units
- **Volume**: cubic feet (individual tree basis, multiply by TPA for per-acre)
- **Biomass**: pounds (dry weight; calibrated against FIA DRYBIO_AG which is in lbs).
  Convert to short tons: lbs ÷ 2000. Convert to kg: lbs ÷ 2.20462.
- **Carbon**: 50% of dry biomass by mass

## Data Sources and Maintenance

### Original Source
The NSBE coefficients are maintained at:
```
/path/to/Documents/MAINE/DATA/Analysis/NSBE/VTECO_modified
```

### Copying to Cardinal/Remote Systems
Files in this directory should be synced to the Cardinal HPC cluster via:

```bash
# From the project root on the development machine:
scp -r data/NSBE/* user@cardinal:/path/to/fvs-modern/data/NSBE/
```

Or set the `NSBE_ROOT` environment variable on Cardinal to point to a shared location.

### Fallback Logic
The R script (20_volume_equations.R) searches for NSBE data in this order:
1. `$NSBE_ROOT` environment variable (if set)
2. `{FVS_PROJECT_ROOT}/data/NSBE` (local project copy)
3. `/path/to/Documents/MAINE/DATA/Analysis/NSBE/VTECO_modified` (source)

## Usage in R

### Basic NSBE Computation

```r
# Load the module
source("calibration/R/20_volume_equations.R")

# Simple per-acre volume from tree list
trees <- data.frame(
  dbh = c(5.2, 12.1, 8.5),           # inches
  ht = c(45, 65, 52),                # feet
  tpa = c(50, 30, 40),               # trees per acre
  spcd = c(12, 12, 108)              # species codes
)

result <- compute_stand_volume_nsbe(trees)
# Returns: cuft_ib, cuft_ob, biomass_total_tons, biomass_bark_tons,
#          biomass_branch_tons, carbon_tons
```

### With Ecodivision

```r
# If ecodivision information available (ECOSUBCD)
result <- compute_nsbe_volume(trees, ecodiv = "M330")
```

### Integration with Projection Engine

```r
# Add volumes to trajectory output from script 17
trajectory <- add_volume_to_trajectory(trajectory)
# Adds: vol_cuft_ib, vol_cuft_ob, bio_tons_total, bio_tons_bark,
#       bio_tons_branch, carbon_tons
```

## Validation and Diagnostics

Check coefficient availability for a set of species:

```r
coverage <- check_nsbe_coverage(c(12, 108, 202))
# Returns data.table showing which species have volib, volob, biomass coefficients
```

## Notes on Coefficients

- Coefficients are calibrated for trees ≥ 1.0" DBH
- Some species may have SPCD_DIVISION entries only; use `check_nsbe_coverage()` to verify
- Ecodivision mapping (ECOSUBCD) should be included in growth trajectory data when possible for maximum accuracy
- Carbon fraction is fixed at 0.50 (dry biomass to carbon); adjust in `compute_nsbe_volume()` if needed

## File Sizes (Reference)

```
REF_SPECIES.csv:              ~125 KB
volib_coefs.csv:              ~35 KB
volob_coefs.csv:              ~23 KB
total_biomass_coefs.csv:       ~15 KB
bark_biomass_coefs.csv:        ~16 KB
branch_biomass_coefs.csv:      ~13 KB
foliage_coefs.csv:             ~17 KB
rcumib_coefs.csv:              ~18 KB
rcumob_coefs.csv:              ~16 KB
equation_forms_and_calls.csv:  ~1 KB
──────────────────────────────────
Total:                         ~188 KB
```

## Implementation Details

See `calibration/R/20_volume_equations.R` for full implementation.

### Key Functions

1. **load_nsbe_coefficients()**: Loads all coefficient tables into data.table format
2. **get_nsbe_coefficient()**: Looks up coefficients with hierarchical fallback
3. **apply_equation()**: Evaluates equation forms 3, 4, 5, 50
4. **compute_nsbe_volume()**: Main computation engine
5. **compute_stand_volume_nsbe()**: Simplified wrapper (no ecodivision)
6. **add_volume_to_trajectory()**: Integrates with projection output
7. **check_nsbe_coverage()**: Validates species coverage

---

**Last Updated**: 2026-04-04
**Data Version**: VTECO_modified (2024)
