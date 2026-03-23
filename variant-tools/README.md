# FVS Variant Template and Creation System

This directory contains a comprehensive template system for creating or recalibrating Forest Vegetation Simulator (FVS) variants. The system is designed to streamline the process of developing new regional variants with calibrated growth and mortality models.

## Contents

### Template Variant (`template_variant/` directory)

Free-form Fortran 90 templates for all core variant components:

* **template_crown.f90** - Crown ratio prediction model with coefficient placeholders
* **template_dgf.f90** - Diameter growth function with site index and competition effects
* **template_morts.f90** - Mortality/survival model with shade tolerance parameters
* **template_htdbh.f90** - Height-diameter relationship with Curtis-Arney equation
* **template_blkdat.f90** - Species coefficient initialization (bark ratios, corrections)
* **template_sitset.f90** - Site index and stand density index management
* **template_PRGPRM.F90** - Program parameters (memory allocation, species count)

Each template file:
* Contains complete, compilable Fortran 90 free-form code
* Includes extensive inline documentation
* Marks all coefficient arrays with TODO comments showing where to insert calibrated values
* Provides equation forms and parameter interpretations
* References data calibration approaches

### Supporting Tools

#### 1. **variant_coefficients_template.xlsx**
   An Excel workbook with sheets for organizing all calibration data:

   * **Species_List** - Map FIA species codes to array indices, track shade tolerance
   * **Crown_Coefficients** - Crown ratio model parameters (BCR1-BCR4) by species
   * **DG_Coefficients** - Diameter growth coefficients (B1, B2, bias corrections)
   * **Mortality_Coefficients** - Survival/mortality equation coefficients and shade tolerance
   * **HT_DBH_Coefficients** - Height-diameter relationship parameters (b0, b1, b2)
   * **Site_Index** - Default site index and SDI maximum values
   * **Instructions** - Comprehensive guide with R code examples

#### 2. **generate_coefficient_template.py**
   Python script to generate the Excel coefficient template.

   Usage:
   ```bash
   python generate_coefficient_template.py [output_file.xlsx]
   ```

   Creates a new Excel workbook with all sheets and formatting.

#### 3. **register_variant.py**
   Validates a completed variant and generates CMake build system integration code.

   Usage:
   ```bash
   python register_variant.py VARIANT_CODE [SOURCE_DIR]
   ```

   Example:
   ```bash
   python register_variant.py ne my_variant_ne/
   ```

   **Performs:**
   * Validates all required Fortran files exist
   * Checks file format validity
   * Generates CMakeLists.txt snippets for build integration
   * Provides integration instructions
   * Creates variant metadata registry

#### 4. **VARIANT_CREATION_GUIDE.md**
   Comprehensive step-by-step guide covering:

   * Overview of the FVS variant system
   * Data requirements for calibration (FIA plots, repeated measures, etc.)
   * Detailed 4-phase workflow:
     - Phase 1: Project setup and data preparation
     - Phase 2: Model calibration (crown, diameter growth, mortality, height-DBH)
     - Phase 3: Implementation in Fortran
     - Phase 4: Validation and documentation
   * Recalibration procedures for existing variants
   * Complete model equations with interpretation
   * Full R code examples for each model:
     - Crown ratio fitting with non-linear regression
     - Diameter growth with mixed-effects models
     - Mortality with logistic regression
     - Height-diameter with Curtis-Arney equation
   * Validation procedures with R code
   * Submission guidelines

## Quick Start: Creating a Variant

### Step 1: Set Up Your Variant Directory

```bash
# Copy template to your variant directory
cp -r template_variant my_variant_xx/
cd my_variant_xx/

# Rename files (remove "template_" prefix)
for f in template_*.f90; do
  mv "$f" "${f#template_}"
done
```

### Step 2: Prepare Calibration Data

```bash
# Download regional FIA data or prepare your inventory
# Organize with columns: plot_id, tree_id, species, year, dbh, height, ...
python generate_coefficient_template.py variant_coefficients.xlsx

# Fill in the Excel workbook with:
# - Your region's species list (with FIA codes)
# - Inventory plot data
# - Repeated measurements for growth analysis
# - Mortality observations
```

### Step 3: Calibrate Models (in R)

Use the R code examples from VARIANT_CREATION_GUIDE.md:

```r
# Crown ratio model
fit_crown <- nls(cr ~ 10*(b1/(1+b2*ba) + b3*(1-exp(b4*dbh))), ...)

# Diameter growth model
fit_dg <- lme(log_dds ~ log_dbh + ba + si, random = ~1|plot, ...)

# Mortality model
fit_mort <- glm(survived ~ log_dbh + ba + rel_height, family=binomial(), ...)

# Height-diameter model
fit_ht <- nls(height ~ 4.5 + exp(b0 + b1/dbh^b2), ...)
```

### Step 4: Update Fortran Templates

Copy your calibrated coefficients into the Fortran arrays:

```fortran
! In crown.f90:
real, parameter :: BCR1(108) = [ 5.63, 6.00, ... ]
real, parameter :: BCR2(108) = [ 0.0047, 0.0053, ... ]
real, parameter :: BCR3(108) = [ 3.52, 0.43, ... ]
real, parameter :: BCR4(108) = [ -0.069, -0.001, ... ]

! In dgf.f90:
real, parameter :: B1(108) = [ 0.00088, 0.00099, ... ]
real, parameter :: B2(108) = [ 0.0603, 0.0817, ... ]

! And so on for all coefficient arrays...
```

### Step 5: Register and Build

```bash
# Validate your variant
python register_variant.py xx my_variant_xx/

# Follow printed integration instructions to add to FVS CMakeLists.txt

# Build FVS with your new variant
cd fvs-modern/build
cmake ..
make -j 4

# Test the variant
./fvs_simulation --variant xx --input test_inventory.txt
```

### Step 6: Validate and Document

* Compare predictions to independent test data
* Run sensitivity analyses
* Document findings in README.md
* Prepare submission package

## File Organization

```
variant-tools/
├── template_variant/              # Fortran template files
│   ├── template_crown.f90
│   ├── template_dgf.f90
│   ├── template_morts.f90
│   ├── template_htdbh.f90
│   ├── template_blkdat.f90
│   ├── template_sitset.f90
│   └── template_PRGPRM.F90
│
├── generate_coefficient_template.py    # Excel template generator
├── register_variant.py                 # Build system integration tool
├── VARIANT_CREATION_GUIDE.md          # Comprehensive creation guide
└── README.md                          # This file
```

## Key Model Equations

### Crown Ratio
```
CR = 10 * (BCR1/(1 + BCR2*BA) + BCR3*(1 - exp(BCR4*DBH)))
```

### Diameter Growth (log scale)
```
ln(DDS) = b0 + b1*ln(DBH) + b2*BA + b3*SI + COR
```

### Mortality (logistic)
```
P(death) = 1 / (1 + exp(-(b0 + b1*ln(DBH) + b2*BA + b3*Rel_Height)))
```

### Height-Diameter (Curtis-Arney)
```
H = 4.5 + exp(b0 + b1/DBH^b2)
```

## Data Requirements

For calibration you need:

* **Minimum 500-1000 trees** with repeated measurements
* **Multiple plots** across your region's environmental gradient
* **5-10 year measurement intervals** for growth analysis
* **Crown ratio measurements** (if fitting crown model)
* **Species identification** using FIA codes
* **Site variables** (elevation, aspect, soil type if available)

**Recommended data sources:**
* USDA Forest Service FIA Program (https://www.fia.fs.fed.us/)
* State forestry agency inventories
* Forest certification program data
* Your own regional forest measurements

## Important Notes

### Parameter Count and Memory

The number of species (MAXSP) affects memory usage:
* 108 species (national standard): ~750 KB base memory
* 60 species (regional): ~450 KB base memory

Changing MAXSP requires recompilation of the entire program.

### Shade Tolerance (VARADJ)

Critical for mortality modeling:
* 0.1-0.3: Shade intolerant (pines, aspens - suffer under overtopping)
* 0.4-0.6: Intermediate tolerance (many hardwoods)
* 0.7-1.0: Shade tolerant (hemlocks, maples - survive in understory)

### Bias Corrections (COR, COR2, HCOR2, RCOR2)

Account for back-transformation bias in log-linear models:
* Typical range: 0.8-1.2
* 1.0 = no bias
* < 1.0 = model overpredicts
* > 1.0 = model underpredicts

## Examples

The `example_variant/` directory contains a complete (but placeholder) variant showing:
* All required file structure
* Template coefficient arrays
* Proper Fortran 90 free-form formatting
* File organization for CMake integration

## Troubleshooting

### Missing Files
Ensure all 7 required files exist in your variant directory:
```
crown.f90, dgf.f90, morts.f90, htdbh.f90,
blkdat.f90, sitset.f90, PRGPRM.F90
```

### Fortran Format
All files should be free-form Fortran 90 (not fixed-form).
* Do not use C or * in column 1 for comments
* Use `!` for all comments
* Line length < 132 characters recommended

### Coefficient Data Types
All coefficient arrays should be REAL type:
* Arrays dimensioned to MAXSP (or 108)
* Use scientific notation for very small values (e.g., 0.00088 or 8.8e-4)

### CMake Integration
Add variant source list to main FVS CMakeLists.txt in the variants section.
Use `${VAR_SOURCES}` CMake variable syntax (note the `$` prefix).

## Further Reading

* **VARIANT_CREATION_GUIDE.md** - Complete step-by-step workflow
* **template_variant/** files - Detailed inline documentation
* FVS User Guide - https://www.fvs.nrs.fs.fed.us/
* Curtis, R.O. (1967) - Classic height-diameter relationships
* Pretzsch, H. (2009) - Forest Dynamics, Growth and Yield

## Support and Feedback

For questions about the variant creation system:
* Consult VARIANT_CREATION_GUIDE.md for detailed explanations
* Review template files for code structure
* Check Excel workbook Instructions sheet for data organization

For FVS system issues:
* FVS Feedback: fvs-feedback@fs.fed.us
* GitHub Issues: https://github.com/USDA-Forest-Service/ForestVegetationSimulator

---

**Variant System Version**: 1.0
**Last Updated**: 2026-03-22
**Created for**: FVS Modernization Project
