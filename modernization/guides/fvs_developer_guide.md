# FVS Developer Onboarding and Architecture Guide

## 1. Welcome and Purpose

### What is FVS?

The **Forest Vegetation Simulator (FVS)** is a family of individual-tree, distance-independent, forest growth simulation models. It is a system of highly integrated analytical tools based on decades of scientific research and practical experience. Since the development of the first model for northern Idaho in 1973, FVS has evolved into a collection of regional 'variants' representing different geographic areas across North America.

FVS can simulate a wide range of silvicultural treatments for most major forest tree species, forest types, and stand conditions. It is maintained and developed by the **Forest Management Service Center (FMSC)** in Fort Collins, Colorado, a division of the USDA Forest Service.

### Historical Significance

* **1973**: Initial model development for northern Idaho
* **Multi-decade evolution**: Expansion to 25+ regional variants covering North America
* **Scientific rigor**: Built on research from USDA Forest Service scientists including Dr. Albert Stage
* **Practical application**: Used for forest planning, management decision support, and research across the continent

### Current Leadership and Contributions

The FVS project is led and maintained by:
* Michael VanDyck - FVS Staff Lead and Product Owner
* Daniel Wagner - FVS Programmer and Repository Maintainer
* Lance David - FVS Senior Programmer
* Erin Smith-Mateja - FVS Forest Biometrician
* Mark Castle - FVS Forest Biometrician
* Matt Diskin - FVS Forest Biometrician
* Nick Crookston - Forest Research Consultant
* Don Robinson - Ecologist and Canadian Variant (BC, ON) Development

Additional information and training materials are available at:
* **Wiki**: https://github.com/USDAForestService/ForestVegetationSimulator/wiki
* **FVS Website**: https://www.fs.usda.gov/fvs/index.shtml
* **Support Email**: sm.fs.fvs-support@usda.gov

---

## 2. Repository Layout

The FVS repository is organized into logical groups: core engine, regional variants, extension modules, build infrastructure, tests, and archive.

### Directory Structure Overview

```
ForestVegetationSimulator-main/
├── Core Engine
│   ├── base/           # Base growth simulation engine (shared by all variants)
│   ├── common/         # Shared include files and global COMMON blocks
│   │   ├── ARRAYS.F77      # Tree attribute arrays (DBH, HT, ICR, PROB, etc.)
│   │   ├── CONTRL.F77      # Control parameters and model state
│   │   ├── PLOT.F77        # Stand-level attributes
│   │   └── [30+ more]      # Other shared data structures
│   └── bin/            # CMake build system configuration
│
├── Regional Variants (Geographic coverage)
│   ├── ak/    AK - Alaska
│   ├── ak/    BC - British Columbia (Canada)
│   ├── ca/    CA - California
│   ├── ie/    IE - Inland Empire (ID/WA/MT/OR)
│   ├── ls/    LS - Lake States (MI/MN/WI)
│   ├── ne/    NE - Northeast (NY/NE/PA/etc.)
│   ├── nc/    NC - North Carolina
│   ├── pn/    PN - Pacific Northwest (WA/OR)
│   ├── sn/    SN - Sierra Nevada (CA)
│   ├── so/    SO - Southern Interior (AZ/NM)
│   ├── ws/    WS - Western Sierra (CA)
│   ├── wc/    WC - West Coast (OR/CA)
│   └── [15+ more]      # Additional regional variants
│
├── Extension Modules
│   ├── fire/       # Fire effects modeling
│   ├── estb/       # Regeneration/establishment subsystem
│   ├── volume/     # NVEL volume integration
│   ├── dbs/        # Database (flat file) output
│   ├── dbsqlite/   # SQLite database output
│   ├── fiavbc/     # FIA-based biomass/carbon calculations
│   ├── mistoe/     # Mistletoe effects
│   ├── covr/       # Crown cover calculations
│   ├── acd/        # Acid deposition
│   ├── wpbr/       # White pine blister rust
│   └── [10+ more]  # Other specialized modules
│
├── Build System
│   └── bin/
│       ├── CMakeLists.txt    # Master CMake configuration
│       └── FVS*_sourceList.txt  # Per-variant source file lists
│
├── Testing
│   ├── tests/
│   │   ├── test.py          # Python file comparison tool
│   │   ├── makefile         # Test runner makefile
│   │   ├── FVSak/           # Variant-specific test inputs/outputs
│   │   ├── FVSbc/
│   │   └── [25+ variant test dirs]
│   └── testSetFromFMSC/     # Official FMSC test datasets
│
├── Documentation
│   ├── README.md
│   ├── Contributing.md
│   ├── changeNotes/         # Historical change logs
│   └── .github/
│       └── CODEOWNERS
│
└── Archive
    └── archive/            # Legacy/deprecated code
```

### Core Engine Directories

**base/** (33,704 lines across 200+ files)
* Contains the shared growth simulation logic used by all variants
* Implements the main FVS loop and cycle machinery
* Provides default implementations of growth functions
* Regional variants override specific functions from base/

**common/** (30+ .F77 include files)
* Global COMMON block definitions (Fortran shared memory)
* Global parameter definitions (PRGPRM.F77)
* Shared data structures accessed by all components
* Examples: ARRAYS.F77 (tree records), CONTRL.F77 (model state), PLOT.F77 (stand data)

**bin/** (CMake build system)
* Master CMakeLists.txt orchestrates builds for all variants
* Generates per-variant CMakeLists.txt files dynamically
* Handles platform-specific compiler settings (VS10, MinGW, Unix)
* Manages linking of Fortran, C, and C++ code

### Regional Variant Directories

Each variant directory (e.g., ak/, ie/, pn/) contains:
* **blkdat.f** - BLOCK DATA initialization (species lists, coefficients, defaults)
* **dgf.f** - Diameter growth function (variant-specific equations)
* **morts.f** - Mortality calculation (species/habitat-specific)
* **htdbh.f** - Height-to-DBH relationship (variant-specific allometry)
* **crown.f** - Crown dimensions and dynamics
* **grinit.f** - Growth initialization (variant setup)
* **estab.f** - Regeneration estimates (if using estb module)
* **common/** - Variant-specific include files (coefficients, parameters)

### Extension Module Directories

* **fire/** - Fire effects on tree mortality and damage
* **estb/** - Natural regeneration/establishment modeling
* **volume/** - Merchantable volume calculations via NVEL
* **dbs/** - Flat-file database output (legacy)
* **dbsqlite/** - Modern SQLite output
* **fiavbc/** - Biomass and carbon calculations using FIA allometry
* **mistoe/** - Mistletoe infestation effects
* **covr/** - Crown cover percentage calculations
* **acd/** - Acid deposition effects
* **wpbr/** - White pine blister rust damage

### Virtual (v*) Directories

The v* directories (vbase/, vestb/, vvolume/, etc.) are symbolic links or copies used during the build process. They can be ignored by developers.

---

## 3. Architecture Overview

### 3.1 Variant-Based Polymorphism Pattern

FVS implements **compile-time polymorphism** using Fortran's INCLUDE mechanism and file organization:

1. **Base Engine** (base/) defines default implementations
2. **Variant Overrides** (e.g., ak/, ie/, pn/) provide variant-specific versions
3. **CMake Build System** selects the correct files per variant

Example: Diameter growth function
```fortran
! base/dgdriv.f - Default diameter growth driver
SUBROUTINE DGDRIV(...)
  ! Generic implementation
END SUBROUTINE DGDRIV

! ak/dgf.f - Alaska variant override
SUBROUTINE DGF(...)
  ! Alaska-specific diameter growth equations
END SUBROUTINE DGF
```

During the build, the CMake system reads `FVSak_sourceList.txt`, which includes:
* Common base routines (algcmp.f, algkey.f, etc.)
* Variant-specific overrides (ak/dgf.f, ak/morts.f, etc.)
* Shared include files (common/*.F77)

The linker resolves to the variant-specific version when both base/ and variant/ define the same subroutine.

### 3.2 Main Simulation Cycle

The core simulation flow is defined in:
* **main.f** - Program entry point
* **base/cmdline.f** - Command-line processing
* **base/fvs.f** - Main simulation loop (subroutine FVS)

#### High-Level Flow:

```
main.f (PROGRAM MAIN)
  ├─ fvsSetCmdLine()          ! Process command-line arguments
  │
  └─ Main Loop: DO FVS() UNTIL return_code != 0
       │
       └─ FVS() subroutine
           ├─ CALL INITRE()        ! Initialize simulation
           ├─ CALL TOPSDI()        ! Calculate stand density index
           │
           ├─ Growth Cycles Loop
           │  ├─ CALL KWORD()       ! Process keywords for this cycle
           │  ├─ CALL GROWS()       ! Run growth model
           │  │  ├─ CALL DG()       ! Diameter growth
           │  │  ├─ CALL HT()       ! Height growth (from height-growth function)
           │  │  ├─ CALL MORTS()    ! Mortality
           │  │  └─ CALL CWIDTH()   ! Crown width update
           │  │
           │  ├─ CALL THIN()        ! Thinning operations (if specified)
           │  ├─ CALL HARV()        ! Harvest operations (if specified)
           │  ├─ CALL ESTBCYC()     ! Regeneration (if using estb module)
           │  ├─ CALL FIRE()        ! Fire effects (if using fire module)
           │  ├─ CALL COMPRS()      ! Compress tree list (remove dead trees)
           │  ├─ CALL UPDATE()      ! Update DBH/HT for next cycle
           │  └─ CALL OUTPUT()      ! Write summary outputs
           │
           └─ RETURN return_code
```

### 3.3 Tree Record Storage: Parallel Arrays

Individual trees are stored as **parallel arrays** in the ARRAYS COMMON block (common/ARRAYS.F77):

```fortran
! From common/ARRAYS.F77
INTEGER DAMSEV(6,MAXTRE), ICR(MAXTRE), IDTREE(MAXTRE), ISP(MAXTRE), ...
REAL DBH(MAXTRE), HT(MAXTRE), DG(MAXTRE), HTG(MAXTRE), PROB(MAXTRE), ...
REAL CFV(MAXTRE), BFV(MAXTRE), CRWDTH(MAXTRE), ...
```

**How it works:**
* MAXTRE = maximum trees in a stand (typically 10,000 or more)
* Each index `i` represents a tree
* `DBH(i)` = diameter at breast height of tree i
* `HT(i)` = height of tree i
* `ISP(i)` = species code of tree i
* `PROB(i)` = probability weight (used in compression)
* `ICR(i)` = increment record number

**Advantages:**
* Cache-friendly; good performance for array operations
* Easy vectorization on some Fortran compilers
* Compatible with fixed-form Fortran 77

**Disadvantages:**
* Difficult to add new per-tree attributes
* No type safety
* All trees must fit in a single monolithic structure

### 3.4 COMMON Block Global State Model

FVS uses Fortran COMMON blocks (global shared memory) extensively. Key blocks:

| Block Name | Location | Purpose |
|-----------|----------|---------|
| ARRAYS | common/ARRAYS.F77 | Tree-level attributes (DBH, HT, species, etc.) |
| PLOT | common/PLOT.F77 | Stand-level data (BA, TPA, elevation, aspect, etc.) |
| CONTRL | common/CONTRL.F77 | Control parameters, cycle state, output flags |
| PRGPRM | common/PRGPRM.F77 | Program-wide constants (MAXTRE, MAXSP, MAXCYC, etc.) |
| OUTCOM | common/OUTCOM.F77 | Output control and summary accumulation |
| WORKCM | common/WORKCM.F77 | Work arrays (temporary calculations) |
| ECON | common/ECON.F77 | Economic parameters (for thinning analysis) |
| PDEN | common/PDEN.F77 | Population density and SDI calculations |
| KEYCOM | common/KEYCOM.F77 | Keyword processing state |

**CONTRL Block (common/CONTRL.F77)** - Key variables:
```fortran
LOGICAL LFIRE, LMORT, LDGCAL(MAXSP), LSTART, LSUMRY
INTEGER ICYC                    ! Current cycle number
INTEGER NCYC                    ! Number of cycles to simulate
INTEGER NUMSP                   ! Number of species in stand
REAL YR                         ! Current year
REAL DBHMIN(MAXSP)              ! Min DBH for merchantable volume
REAL BAMAX, BAMIN               ! Max/min basal area thresholds
CHARACTER*2 VARACD              ! Variant code (AK, IE, PN, etc.)
```

### 3.5 Keyword Processing System

FVS is driven by keywords (text commands) that specify model behavior. Keywords control:
* Stand initialization (species, stocking, age)
* Thinning (intensity, type, cycle)
* Harvesting (when, how much, where)
* Outputs (what to report, format)
* Fire, insects, regeneration (module-specific)

#### How Keywords Work:

1. **Keyword File Input** (KWORD.F77)
   * User provides keyword file (e.g., `mystand.key`)
   * Keywords are read and parsed each cycle

2. **KWORD() Subroutine** (base/algkey.f)
   * Parses keyword lines
   * Sets corresponding COMMON block variables
   * Calls module-specific keyword handlers

3. **Module Integration**
   * Each module (fire, estb, etc.) has its own keyword processor
   * Keywords enable/disable specific calculations

Example keyword file:
```
COMPUTE
  STAND AK 100
  TREE 5.0 0.1
  SPECIES SF AF YC
  CYCLE 1
  THIN PARMS 0.5 40 0.0 0.0 1 1 1
  HARVEST REMOVE 500 TREES
  END
STOP
```

---

## 4. How a Simulation Run Works: Step by Step

### Entry Point: main.f

```fortran
PROGRAM MAIN
  INTEGER rtnCode, lenCL, i

  ! 1. Process command line (fetch actual args if empty string passed)
  lenCL = 0
  CALL fvsSetCmdLine(' ', lenCL, rtnCode)
  IF (rtnCode .NE. 0) GOTO 10

  ! 2. Run all cycles and stands until stop point
  DO
    CALL FVS(rtnCode)           ! Returns non-zero when finished
    IF (rtnCode .NE. 0) EXIT
  ENDDO

  ! 3. Exit with appropriate code
  10 CONTINUE
  CALL fvsGetICCode(i)
  IF (i .EQ. 0) STOP
  GO TO (11, 12, 13, 14, 15), i
  ! ... handle various exit codes ...
END PROGRAM MAIN
```

### Execution Phase 1: Initialization (INITRE)

Called once per stand:

```
INITRE()
  ├─ Initialize species arrays (species list for variant)
  ├─ Initialize tree arrays (clear, allocate)
  ├─ Read stand data (elevation, aspect, slope, forest type)
  ├─ Read initial tree inventory (DBH, HT, species, probability)
  ├─ Calculate site index (site quality assessment)
  ├─ Set default parameters (DBHMIN, BAMAX, etc.)
  ├─ Initialize output files/databases
  └─ LSTART = TRUE (mark initialization complete)
```

### Execution Phase 2: Growth Cycling (Main Loop)

For each cycle from 1 to NCYC:

#### 2a. Keyword Processing (KWORD)
```
KWORD()
  ├─ Read keyword records for this cycle
  ├─ Parse THIN, HARVEST, ESTABLISH, COMPUTE keywords
  ├─ Set control flags (LTHIN, LHARV, LFIRE, etc.)
  ├─ Thinning specs: efficiency, target BA, target DBH
  └─ Harvest specs: volume removal, merchantability limits
```

#### 2b. Growth Calculation (GROWS)
```
GROWS()
  For each tree i in stand (1 to NTRE):
    ├─ Check if tree is alive and above minimum DBH
    ├─ Calculate DBH increment: DG(i) = DGF(DBH, HT, species, ...)
    ├─ Calculate height increment: HTG(i) = HTF(DBH, HT, species, ...)
    ├─ Calculate mortality: MORTS() checks if tree dies
    ├─ Update crown dimensions: CWIDTH()
    └─ Accumulate growth for output
```

**Diameter Growth (DGF):** Variant-specific equations using
* Current DBH and height
* Species-specific coefficients (in variant blkdat.f)
* Basal area per acre (BA)
* Stand density index (SDI)
* Habitat type, site index, elevation

Example:
```
DG = (BETA0 + BETA1*BA + BETA2*DBH + BETA3*ELEV) * PROB
```

**Height Growth (HTG):** Often model-specific; examples:
* Wykoff method
* Curtis method
* Richards S-curve

**Mortality (MORTS):** Variant-specific; considers
* Self-thinning density-dependent mortality
* Species-specific maximum age
* Crown ratio (live crown / total height)
* Density thresholds

#### 2c. Thinning (THIN)
If LTHIN is set:
```
THIN()
  ├─ Sort trees by DBH (largest first) or criteria
  ├─ Mark trees for removal until threshold met
  │  (efficiency target, BA target, etc.)
  ├─ Set PROB(i) = 0 for removed trees
  └─ Accumulate removed volume/stems
```

#### 2d. Harvesting (HARV)
If LHARV is set:
```
HARV()
  ├─ Check merchantability by species and DBH
  ├─ Select trees for removal (priority: large first)
  ├─ Calculate volume per tree: CFV, BFV (using VOLS)
  ├─ Set PROB(i) = 0 for harvested trees
  └─ Accumulate removed volume and cost
```

#### 2e. Regeneration (ESTBCYC)
If estb module is enabled:
```
ESTBCYC()
  ├─ Check if establishment is scheduled
  ├─ Calculate microsites available (based on tree mortality, harvest)
  ├─ Simulate seed arrival and germination
  ├─ Establish new seedlings
  └─ Add new trees to array (small initial DBH/HT)
```

#### 2f. Fire Effects (FIRE)
If fire module is enabled:
```
FIRE()
  ├─ Check if fire is scheduled
  ├─ Calculate ignition probability and spread
  ├─ Damage trees based on bark thickness, species
  ├─ Increment damage severity (DAMSEV array)
  └─ Mark severely damaged trees for mortality
```

#### 2g. Compression (COMPRS)
Critical step: removes dead trees and optimizes storage:
```
COMPRS()
  ├─ Iterate through tree array
  ├─ For each dead tree (PROB = 0):
  │  ├─ Move last live tree to its position
  │  └─ Decrement tree count
  ├─ Reset all non-essential arrays (WK1-WK15)
  └─ Return updated tree count (NTRE)
```

#### 2h. Update (UPDATE)
Prepare for next cycle:
```
UPDATE()
  ├─ Add DG to DBH: DBH(i) = DBH(i) + DG(i)
  ├─ Add HTG to HT: HT(i) = HT(i) + HTG(i)
  ├─ Clear growth arrays: DG() = 0, HTG() = 0
  ├─ Increment cycle counter: ICYC = ICYC + 1
  ├─ Increment year: YR = YR + 1
  └─ Recalculate stand-level stats: BA, TPA, AVH, etc.
```

#### 2i. Output (OUTPUT)
Write results for this cycle:
```
OUTPUT()
  ├─ Calculate stand summary (BA, TPA, QMD, etc.)
  ├─ If JOSUM output flag set:
  │  └─ Write summary record to SUMTAB
  ├─ If JOTREE output flag set:
  │  └─ Write individual tree records to TREETAB
  ├─ If JOLIST output flag set:
  │  └─ Print line printer output (deprecated)
  └─ Update database (if dbs or dbsqlite modules)
```

### Execution Phase 3: Stand Completion

```
FVS() returns when:
  ├─ All NCYC cycles complete, or
  ├─ STOP keyword encountered, or
  └─ Fatal error occurred (insufficient memory, etc.)
```

---

## 5. Adding or Modifying a Regional Variant

### Prerequisites

Each variant is a complete growth model for a geographic region. To create or modify a variant, understand:
* The forest types and species in the region
* Research on growth and mortality from that region
* Available growth equations or data

### Required Files

A variant directory (e.g., `pn/`) must contain:

| File | Purpose |
|------|---------|
| **blkdat.f** | BLOCK DATA initialization; species list, allometric coefficients, default parameters |
| **dgf.f** | Diameter growth function; variant-specific equations |
| **morts.f** | Mortality model; species/density-dependent mortality |
| **htdbh.f** | Height-to-DBH relationship; dominant height-to-diameter curve |
| **crown.f** | Crown dimensions; crown width and crown ratio functions |
| **grinit.f** | Growth initialization; variant-specific setup |
| **estab.f** | (Optional) Establishment/regeneration model |
| **common/*.F77** | Variant-specific include files with equation coefficients |

### Example: blkdat.f Structure

From `/home/aweiskittel/ForestVegetationSimulator-main/ak/blkdat.f`:

```fortran
BLOCK DATA BLKDAT
  IMPLICIT NONE

  INCLUDE 'PRGPRM.F77'    ! Global parameters
  INCLUDE 'ESPARM.F77'    ! Establishment parameters
  INCLUDE 'COEFFS.F77'    ! Diameter growth coefficients
  INCLUDE 'CONTRL.F77'    ! Control parameters
  ! ... 20+ more include files ...

  ! Species list initialization (example from Alaska variant)
  !    Number  Code  Common Name       FIA  Scientific Name
  ! 1  SF   Pacific silver fir      011  Abies amabilis
  ! 2  AF   Subalpine fir           019  Abies lasiocarpa
  ! 3  YC   Alaska cedar            042  Callitropsis nootkatensis
  ! ...

  ! Initialize diameter growth coefficients
  ! DGF_BETA0(sp), DGF_BETA1(sp), DGF_BETA2(sp), etc.

END BLOCK DATA BLKDAT
```

### How to Override Base Routines

1. **Understand Base Behavior**
   Read the base implementation, e.g., `base/dgdriv.f`

2. **Create Variant Version**
   Create `pn/dgf.f` with the same subroutine name but variant-specific equations

3. **CMake Picks Variant File**
   During build, CMake reads `FVSpn_sourceList.txt`:
   ```
   ../../base/dgdriv.f     (or omit this line)
   ../../pn/dgf.f          (this version is linked instead)
   ```

4. **Linker Resolution**
   The variant-specific object file (pn/dgf.o) overrides the base file

### Organizing Coefficients in blkdat.f

Use variant-specific include files for equation coefficients:

```fortran
! In pn/common/DGF_COEFF.F77
REAL DGF_BETA0(MAXSP), DGF_BETA1(MAXSP), DGF_BETA2(MAXSP)
REAL DGF_BETA3(MAXSP), DGF_BETA4(MAXSP)

COMMON /DGF_COEFF/ DGF_BETA0, DGF_BETA1, DGF_BETA2, DGF_BETA3, DGF_BETA4

DATA DGF_BETA0 /  0.5,  0.4,  0.3, ... /  ! Pacific NW values
DATA DGF_BETA1 / -0.02, -0.01, -0.015, ... /
! ... more data statements ...
```

Then in `pn/blkdat.f`:
```fortran
INCLUDE 'DGF_COEFF.F77'
! Data statements initialize coefficients at program load
```

### How CMake Builds a Variant

1. **Generate Source List**
   CMake reads `FVSpn_sourceList.txt` (e.g., from bin/FVSpn_sourceList.txt)

2. **Create Build Directory**
   `bin/CMakeLists.txt` creates `pn_CmakeDir/`

3. **Compile and Link**
   ```bash
   cd pn_CmakeDir/
   cmake -G "Unix Makefiles" ..
   make
   # Produces: libFVS_pn.so (Linux) or FVSpn.dll (Windows)
   ```

4. **Link Executable**
   Links libFVS_pn.so + libFVSsql.so + libFVSfofem.so → FVSpn executable

---

## 6. Extension Module Architecture

### 6.1 Fire Effects Module (fire/)

**Purpose**: Simulate fire-caused tree mortality and damage accumulation.

**File structure**:
```
fire/
├── base/          (fire module core, shared)
├── ak/            (variant-specific fire implementation)
├── pn/
└── [other variants]
```

**Integration with growth cycle**:
1. FIRE keyword triggers fire scheduling
2. Each cycle: FIRE() checks if fire is scheduled
3. Fire damage calculated and applied to trees
4. Damaged trees added to DAMSEV array
5. Damaged trees may experience elevated mortality in MORTS()

**Key data structures**:
```fortran
DAMSEV(6, MAXTRE)    ! Damage severity codes by damage type and tree
DEFECT(MAXTRE)       ! Volume defect percentage
```

**Example fire keyword**:
```
FIRE STAND 2023 PROBABILITY 0.5 SEVERITY HIGH
```

### 6.2 Regeneration Module (estb/)

**Purpose**: Model natural tree regeneration and seedling establishment.

**File structure**:
```
estb/
├── esadsub.f         (advancement: move seedlings → small trees)
├── esdlay.f          (delay germination/establishment)
├── esdistr.f         (distribute regeneration by density)
├── esgent.f          (seed germination)
├── esnspe.f          (non-stocked area regeneration)
├── espsub.f          (post-harvest regeneration prep)
├── esprep.f          (regeneration preparation)
└── [25+ supporting subroutines]
```

**Integration with growth cycle**:
```
ESTBCYC() is called each cycle if estb enabled
  ├─ Check regeneration conditions
  ├─ Establish new seedlings in vacancies
  ├─ Advance small trees
  └─ Return when all regeneration complete
```

**Key parameters** (estb/common/ESPARM.F77):
```fortran
! Regeneration state and delay periods
LOGICAL LESAB, LESDEN, LESPLT
INTEGER ESTDLY(MAXSP), ESTRDY(MAXSP)
REAL ESPRD(MAXSP), ESDEN(MAXSP)
```

**Example establishment keyword**:
```
ESTABLISH MIXTURE 0.4 PP 0.3 DF 0.3
DELAY 2 CYCLES
```

### 6.3 Volume Module (volume/)

**Purpose**: Calculate merchantable timber volumes using NVEL (National Volume Estimator Library).

**File structure**:
```
volume/
├── base/            (shared NVEL integration)
└── [variant-specific overrides]
```

**Integration with growth cycle**:
1. After growth update, VOLS() called for each tree
2. Calculates: CFV (cubic feet), BFV (board feet), MCFV (merchantable)
3. Stores in tree arrays for output and harvest decisions

**Key subroutines**:
```fortran
CALL VOLS(...)      ! Calculate volumes for a tree
CALL NVEL(...)      ! Call National Volume Estimator
```

**Example volume data**:
```fortran
CFV(i)   = Total cubic foot volume per tree
BFV(i)   = Board foot volume per tree
MCFV(i)  = Merchantable cubic feet (above minimum DBH)
SCFV(i)  = Sawtimber cubic feet volume
```

### 6.4 Database Output Modules (dbs/, dbsqlite/)

#### dbs/ - Legacy Flat File Output

**Purpose**: Write results to comma-separated or fixed-format files.

**Files**:
```
dbs/
├── database.f       (flat file database interface)
└── [supporting routines]
```

#### dbsqlite/ - Modern SQLite Output

**Purpose**: Write results to SQLite database (queryable, efficient).

**Files**:
```
dbsqlite/
├── fvsqlite3.c      (SQLite library wrapper)
├── apisubsc.c       (API for database writing)
└── [supporting C/C++ code]
```

**Integration**:
1. OUTPUT() called each cycle
2. If SQLite enabled: write tree/stand records to .db file
3. Multiple cycles/stands accumulate in single database

**Output tables**:
```
SUMTAB          - Stand summary (by cycle)
TREETAB         - Individual tree details (by cycle)
TREESB          - Tree standing/dead/harvested classification
FVSTREATM       - Treatment history
```

---

## 7. Build System

### 7.1 CMake Structure

**Master file**: `bin/CMakeLists.txt`

**How it works**:
1. Scans for `FVS*_sourceList.txt` files
2. For each variant:
   * Extracts source files
   * Creates per-variant build directory
   * Generates platform-specific CMakeLists.txt
   * Invokes cmake recursively

```cmake
# From bin/CMakeLists.txt
file(GLOB tobuild FVS*_sourceList.txt)
foreach (sourceList ${tobuild})
  # Parse variant name from sourceList filename
  # Create variant_CmakeDir/CMakeLists.txt
  # Recursively call cmake in that directory
endforeach(sourceList)
```

### 7.2 Per-Variant Build

**Location**: `bin/{variant}_CmakeDir/CMakeLists.txt` (auto-generated)

**Key steps**:

```cmake
project(FVS C CXX Fortran)

set(CMAKE_Fortran_Format FIXED)    # Fortran 77 fixed-form

# Read source file list
file(STRINGS ${prgName}_sourceList.txt fileList)

# Separate into libraries and executable
foreach(fn ${fileList})
  # Categorize: .f (Fortran), .c/.cpp (C/C++), .h/.F77 (headers)
endforeach(fn)

# Create shared libraries
add_library(FVSsql SHARED ${CsourceSQL})       # SQLite
add_library(FVSfofem SHARED ${CsourceFFE})     # Fire/Other modules
add_library(FVS_${varName} SHARED ${FsourceFVS})  # Variant-specific

# Create executable
add_executable(${prgName} ${FsourceMAIN})

# Link everything
target_link_libraries(${prgName}
  FVS_${varName} FVSsql FVSfofem)
```

### 7.3 Platform-Specific Compiler Settings

**Visual Studio 10 (Windows)**:
```cmake
set(CMAKE_C_FLAGS "/D_WINDOWS /W3 /Zm100")
add_definitions(-DANSI -DWINDOWS -D_WINDLL)
```

**MinGW (Windows/Linux cross-compile)**:
```cmake
add_definitions(-DANSI -DWINDOWS -D_WINDLL -DCMPgcc)
set_target_properties(FVSfofem PROPERTIES
  LINK_FLAGS -Wl,--add-stdcall-alias)
```

**Unix/Linux (gfortran)**:
```cmake
add_definitions(-DANSI -DCMPgcc)
```

### 7.4 Building on Different Platforms

#### Linux (gfortran)

```bash
cd ForestVegetationSimulator-main/bin
cmake -G "Unix Makefiles" .
# Creates: pn_CmakeDir/, ak_CmakeDir/, etc.
cd pn_CmakeDir
make -j4
# Produces: FVSpn executable
```

#### Windows (Visual Studio + Intel Fortran)

```bash
cd bin
cmake -G "Visual Studio 10 Win64" .
cd pn_CmakeDir
cmake --build . --config Release
# Produces: FVSpn.exe
```

#### macOS (clang, gfortran via Homebrew)

```bash
cd bin
cmake -G "Unix Makefiles" .
cd pn_CmakeDir
make -j4
# Produces: FVSpn executable (x86_64 or arm64)
```

### 7.5 Debug vs Release Builds

**Release Build** (default):
```bash
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release .
make
```

**Debug Build**:
```bash
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug .
make
# Includes debug symbols, no optimization
```

---

## 8. Testing

### 8.1 Current Test Infrastructure

FVS uses a simple file-comparison test framework.

**Test files**:
* `tests/test.py` - Python script comparing output files
* `tests/makefile` - Orchestrates test runs

**Test structure**:
```
tests/
├── FVSak/           # Alaska variant tests
│   ├── input.key    # Keyword file
│   ├── input.csv    # Initial tree data
│   └── expected/    # Expected outputs
│       ├── summary.csv
│       └── trees.csv
├── FVSbc/           # British Columbia variant tests
├── FVScr/
└── [25+ variant test directories]
```

### 8.2 Running Tests

**Run all tests**:
```bash
cd tests
python test.py  # Runs all variants
```

**Run single variant**:
```bash
cd tests/FVSak
make test
```

**Test comparison logic** (test.py):
```python
import filecmp
import sys

if filecmp.cmp(sys.argv[1], sys.argv[2]):
    print("Comparison Test Passed")
else:
    print("Comparison Test Failed")
```

### 8.3 How to Add New Test Cases

1. **Create test directory**:
   ```bash
   mkdir -p tests/FVSxx/  # xx = variant code
   ```

2. **Add input files**:
   ```
   tests/FVSxx/
   ├── input.key        (keyword file)
   ├── input.csv        (initial inventory)
   └── expected/        (expected outputs)
       ├── summary.csv
       └── trees.csv
   ```

3. **Create Makefile**:
   ```makefile
   test:
       $(FVSxx) < input.key > output.csv
       python ../test.py output.csv expected/summary.csv
   ```

4. **Run test**:
   ```bash
   cd tests/FVSxx
   make test
   ```

### 8.4 Testing Improvements: pFUnit

The modernization roadmap includes converting to **pFUnit** (Fortran unit testing framework).

See: `modernization/guides/pfunit_testing_guide.md` for the pFUnit integration guide.

**Benefits of pFUnit**:
* True unit tests on individual subroutines
* Automated test discovery and execution
* Better integration with CI/CD
* Enables refactoring with confidence

---

## 9. Code Conventions and Style

### 9.1 Fortran 77 Fixed-Form Format

FVS is written in **Fortran 77 fixed-form**, a legacy format with strict column rules:

```
Column 1:        ' ' = regular statement, 'C' = comment, '*' = comment
Columns 2-5:     Statement label (line number) or blank
Column 6:        Continuation character (non-blank = continuation of previous line)
Columns 7-72:    Fortran statement
Columns 73-80:   Card sequence number (ignored, legacy)
```

**Example**:
```fortran
C Column counting:   123456789012345678901234567890123456789012345678901234567890123456789012
      SUBROUTINE GROWS
      IMPLICIT NONE
C
C     THIS SUBROUTINE CALCULATES GROWTH INCREMENTS
C
      INTEGER I, J, K
      REAL DBH(1000), HT(1000), DG(1000)
C
      DO 10 I = 1, 100
        IF (DBH(I) .LT. 1.0) GOTO 10
        DG(I) = 0.5 * DBH(I) + 0.1 * HT(I)
        DBH(I) = DBH(I) + DG(I)
   10 CONTINUE
C
      RETURN
      END
```

**Fixed-form rules**:
* Column 6 non-blank = continuation of previous line
* Statements must start in column 7+
* No more than 72 characters per line
* Cannot use lowercase easily (converted to uppercase)

### 9.2 Variable Naming Conventions

FVS follows Fortran implicit typing traditions:

**Integer variables** (A-H, N-Z, or explicit INTEGER declaration):
```fortran
ITRE, ICYC, NUMSP, IDTREE, ISP, ICR
```

**Real variables** (A-H, N-Z, or explicit REAL declaration):
```fortran
DBH, HT, DG, BA, TPA, CFV, BFV
```

**Logical variables** (LOGICAL declaration):
```fortran
LMORT, LFIRE, LSTART, LTHIN, LHARV
```

**Character variables** (CHARACTER declaration):
```fortran
VARACD              ! Variant code (2 chars)
KWDFIL              ! Keyword file name
TREFMT              ! Tree format
```

**Array suffixes**:
* `(MAXSP)` = per-species array
* `(MAXTRE)` = per-tree array
* `(MAXCYC)` = per-cycle array
* `(MAXPLT)` = per-plot array

### 9.3 Comment Conventions

```fortran
C     This is a full-line comment starting in column 7

      CALL GROWS()  ! This is an end-of-line comment

C----------
C Some comments use dashes as visual separators
C----------

C     VARIABLE DEFINITIONS:
C     DBH  -- Tree diameter at breast height (inches)
C     BA   -- Basal area per acre (square feet)
C     TPA  -- Trees per acre
```

### 9.4 COMMON Block Include Patterns

All COMMON blocks are in `common/*.F77` include files, following a standard pattern:

```fortran
C----------
C CODE SEGMENT ARRAYS
C----------
C
      LOGICAL LBIRTH(MAXTRE)
C
      INTEGER IDTREE(MAXTRE), ISP(MAXTRE), ...
      REAL DBH(MAXTRE), HT(MAXTRE), DG(MAXTRE), ...
C
      COMMON /ARRAYS/ IDTREE, ISP, DBH, HT, DG, LBIRTH, ...
C
C----------
C VARIABLE DEFINITIONS:
C----------
C    LBIRTH -- Logical array indicating if tree is a seedling
C    IDTREE -- Tree ID number
C    ISP    -- Species code (1 to NUMSP)
C    DBH    -- Diameter at breast height (inches)
```

**In source files, include as**:
```fortran
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'
```

---

## 10. Modernization Roadmap

The FVS codebase is undergoing modernization to improve maintainability, testability, and developer experience. Key initiatives:

### 10.1 Free-Form Fortran Conversion

**Goal**: Convert from Fortran 77 fixed-form to modern Fortran 90+ free-form.

**Benefits**:
* More readable code (no column constraints)
* Easier to maintain and debug
* Better compiler support (fewer warnings)
* Can use modern Fortran features (modules, derived types)

**Reference**: See `modernization/guides/` for the free-form conversion script and best practices.

**Conversion script strategy**:
* Automated conversion of fixed-form to free-form
* Preserve all functionality
* Handle continuation lines, comments, etc.
* Create mapping of converted files

**Example conversion**:
```fortran
! Before (fixed-form): fvs77.f
      PROGRAM MAIN
      IMPLICIT NONE
C
      INTEGER ICYC, NCYC
      REAL DBH(1000), HT(1000), DG(1000)
C
      DO 10 ICYC = 1, NCYC
        CALL GROWS(DBH, HT, DG)
   10 CONTINUE
C
      END PROGRAM MAIN

! After (free-form): fvs90.f90
program main
  implicit none

  integer :: icyc, ncyc
  real :: dbh(1000), ht(1000), dg(1000)

  do icyc = 1, ncyc
    call grows(dbh, ht, dg)
  end do

end program main
```

### 10.2 Module Migration

**Goal**: Convert COMMON blocks to Fortran modules with encapsulation.

**Benefits**:
* Type safety (no implicit typing)
* Encapsulation (PRIVATE attributes)
* Namespace management
* Better IDE support

**Roadmap**:
1. Convert PRGPRM.F77 → prgprm.f90 (module prgprm)
2. Convert ARRAYS.F77 → arrays.f90 (module arrays)
3. Convert CONTRL.F77 → contrl.f90 (module contrl)
4. Gradually migrate all common blocks

**Example**:
```fortran
! Before (COMMON block): ARRAYS.F77
      INTEGER IDTREE(MAXTRE), ISP(MAXTRE)
      REAL DBH(MAXTRE), HT(MAXTRE)
      COMMON /ARRAYS/ IDTREE, ISP, DBH, HT

! After (Fortran module): arrays.f90
module arrays
  use prgprm, only: MAXTRE
  implicit none
  private

  integer, allocatable, public :: idtree(:), isp(:)
  real, allocatable, public :: dbh(:), ht(:)

contains

  subroutine arrays_init()
    allocate(idtree(MAXTRE), isp(MAXTRE))
    allocate(dbh(MAXTRE), ht(MAXTRE))
  end subroutine arrays_init

end module arrays
```

### 10.3 GOTO Elimination

**Goal**: Remove all GOTO statements and replace with structured control flow.

**Benefits**:
* Code readability
* Easier to understand control flow
* Fewer spaghetti code anti-patterns

**Roadmap**:
1. Catalog all GOTO statements (see `modernization/` for catalog)
2. Convert to DO loops, IF/THEN/ELSE, and subroutine calls
3. Refactor complex routines with nested GOTOs

**Example GOTO elimination**:
```fortran
! Before (with GOTO)
      INTEGER I, J
      DO 100 I = 1, 1000
        DO 100 J = 1, 1000
          IF (DBH(I) .LT. 1.0) GOTO 100
          CALL GROWS(I, J)
  100 CONTINUE

! After (structured)
      INTEGER I, J
      DO I = 1, 1000
        DO J = 1, 1000
          if (DBH(I) >= 1.0) then
            call grows(I, J)
          end if
        end do
      end do
```

### 10.4 Testing Improvements (pFUnit Integration)

**Goal**: Implement unit tests for individual subroutines.

**Reference**: See `modernization/guides/pFUnit_integration.md`

**Roadmap**:
1. Set up pFUnit test framework
2. Create test suites for critical modules (growth, mortality, etc.)
3. Integrate with CI/CD pipeline
4. Achieve >80% code coverage

**Example pFUnit test**:
```fortran
module test_grows
  use pFUnit
  implicit none

contains

  @test
  subroutine test_diameter_growth()
    real :: dbh, dg, ht
    dbh = 10.0
    ht = 60.0
    call dgf(dbh, dg, ht, ...)
    @assertEqual(0.5, dg, tolerance=0.01, message="DG out of range")
  end subroutine test_diameter_growth

end module test_grows
```

### 10.5 CI/CD Setup (GitHub Actions)

**Goal**: Automate building and testing on every push.

**Reference**: See `.github/` directory (and future GitHub Actions guides).

**Roadmap**:
1. Create GitHub Actions workflow files (`.github/workflows/`)
2. Test on Linux (gfortran), Windows (MinGW), macOS (gfortran)
3. Run full test suite on all variants
4. Generate coverage reports
5. Deploy releases to GitHub Releases

**Example workflow** (`.github/workflows/build.yml`):
```yaml
name: Build FVS

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install gfortran
        run: sudo apt-get install gfortran
      - name: Build
        run: |
          cd bin
          cmake -G "Unix Makefiles" .
          cd pn_CmakeDir
          make -j4
      - name: Run tests
        run: |
          cd tests
          python test.py
      - name: Upload artifacts
        uses: actions/upload-artifact@v2
        with:
          name: fvs-executables
          path: bin/*/FVS*
```

### 10.6 Refactoring Priorities

**Phase 1** (Foundation):
* Free-form conversion of core files (base/main.f, base/fvs.f, base/algkey.f)
* Establish pFUnit test framework
* Set up GitHub Actions CI

**Phase 2** (Core Modules):
* Convert ARRAYS.F77, CONTRL.F77, PLOT.F77 to modules
* GOTO elimination in high-frequency routines
* Unit tests for growth, mortality, volume functions

**Phase 3** (Expansion):
* All remaining COMMON blocks → modules
* Remove all GOTO statements
* Full pFUnit coverage of all variants

**Phase 4** (Polish):
* Performance optimization (now that code is modernized)
* Documentation updates
* API stability and backward compatibility

---

## Appendix A: File Cross-Reference

### Main Execution Flow Files
* `/home/aweiskittel/ForestVegetationSimulator-main/base/main.f` - Entry point
* `/home/aweiskittel/ForestVegetationSimulator-main/base/cmdline.f` - Command-line processing
* `/home/aweiskittel/ForestVegetationSimulator-main/base/fvs.f` - Main simulation loop
* `/home/aweiskittel/ForestVegetationSimulator-main/base/algkey.f` - Keyword processing

### Growth Calculation Files
* `/home/aweiskittel/ForestVegetationSimulator-main/base/algcmp.f` - Compression (dead tree removal)
* `/home/aweiskittel/ForestVegetationSimulator-main/base/algevl.f` - Evaluation/growth cycling
* `/home/aweiskittel/ForestVegetationSimulator-main/base/algexp.f` - Expansion
* `/home/aweiskittel/ForestVegetationSimulator-main/base/algspp.f` - Species processing

### Common Block Definitions
* `/home/aweiskittel/ForestVegetationSimulator-main/common/PRGPRM.F77` - Global parameters (MAXTRE, MAXSP, MAXCYC)
* `/home/aweiskittel/ForestVegetationSimulator-main/common/ARRAYS.F77` - Tree attribute arrays
* `/home/aweiskittel/ForestVegetationSimulator-main/common/CONTRL.F77` - Control state and parameters
* `/home/aweiskittel/ForestVegetationSimulator-main/common/PLOT.F77` - Stand-level data
* `/home/aweiskittel/ForestVegetationSimulator-main/common/OUTCOM.F77` - Output control
* `/home/aweiskittel/ForestVegetationSimulator-main/common/PDEN.F77` - Population density

### Variant Structure (Example: Alaska - ak/)
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/blkdat.f` - Initialization and coefficients
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/dgf.f` - Diameter growth (variant-specific)
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/morts.f` - Mortality (variant-specific)
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/htdbh.f` - Height-DBH relationship
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/crown.f` - Crown dimensions
* `/home/aweiskittel/ForestVegetationSimulator-main/ak/grinit.f` - Growth initialization

### Build System Files
* `/home/aweiskittel/ForestVegetationSimulator-main/bin/CMakeLists.txt` - Master CMake configuration
* `/home/aweiskittel/ForestVegetationSimulator-main/bin/FVS*_sourceList.txt` - Per-variant source lists

### Testing Files
* `/home/aweiskittel/ForestVegetationSimulator-main/tests/test.py` - File comparison test script
* `/home/aweiskittel/ForestVegetationSimulator-main/tests/makefile` - Test orchestration
* `/home/aweiskittel/ForestVegetationSimulator-main/tests/FVS*/ ` - Variant-specific test directories

### Documentation
* `/home/aweiskittel/ForestVegetationSimulator-main/README.md` - Project overview
* `/home/aweiskittel/ForestVegetationSimulator-main/Contributing.md` - Contribution guidelines
* `/home/aweiskittel/ForestVegetationSimulator-main/.github/CODEOWNERS` - Maintainer list

---

## Appendix B: Glossary

* **ARRAYS** - COMMON block containing all per-tree attributes (DBH, HT, species, etc.)
* **BA** - Basal area (square feet per acre)
* **BLKDAT** - BLOCK DATA subroutine initializing coefficients and species data
* **CMake** - Cross-platform build system used by FVS
* **COMMON** - Fortran global shared memory (deprecated but used throughout FVS)
* **CONTRL** - COMMON block containing control state and parameters
* **Compression** - COMPRS() subroutine removing dead trees from array
* **DBH** - Diameter at breast height (4.5 feet, in inches)
* **DG** - Diameter growth increment (inches per cycle)
* **DGF** - Diameter growth function (variant-specific equations)
* **FMSC** - Forest Management Service Center (USDA Forest Service)
* **Fortran 77** - Legacy Fortran language (fixed-form used by FVS)
* **FVS** - Forest Vegetation Simulator (the model)
* **HT** - Tree height (feet)
* **HTG** - Height growth increment (feet per cycle)
* **ICR** - Increment record number (for tracking growth history)
* **INITRE** - Initialization subroutine (reads stand data)
* **Keyword** - Text command controlling simulation behavior
* **KWORD** - Keyword processing subroutine
* **MAXCYC** - Maximum simulation cycles (typically 50+)
* **MAXSP** - Maximum species (typically 30-50 per variant)
* **MAXTRE** - Maximum trees in memory (typically 10,000+)
* **Mortality** - Tree death (calculated each cycle in MORTS)
* **MORTS** - Mortality function (variant-specific)
* **NVEL** - National Volume Estimator Library (volume calculation)
* **OUTCOM** - COMMON block containing output state
* **PLOT** - COMMON block containing stand-level data
* **PROB** - Probability weight for tree (compression factor)
* **Prognosis** - Original name for growth model; still used colloquially
* **PRGPRM** - COMMON block containing global parameters
* **QMD** - Quadratic mean diameter (average diameter weighted by basal area)
* **SDI** - Stand density index (self-thinning threshold)
* **Species** - Tree species code (2-4 letters: DF=Douglas-fir, WF=White fir, etc.)
* **Thinning** - Removal of trees (user-directed or automatic)
* **TPA** - Trees per acre
* **VARIANT** - Regional growth model (AK, IE, PN, WS, etc.)
* **Zeide** - SDI calculation method (alternative to Reineke)

---

## Appendix C: Common Development Tasks

### Adding a New Growth Equation

1. Identify variant (e.g., `pn/`)
2. Edit `pn/dgf.f` (diameter growth function)
3. Add new BETA coefficients in `pn/common/DGF_COEFF.F77`
4. Initialize coefficients in `pn/blkdat.f`
5. Rebuild variant: `cd bin/pn_CmakeDir && make`
6. Test with sample stands

### Extending to a New Geographic Region

1. Create new variant directory: `mkdir newregion/`
2. Copy template files from existing variant (e.g., from `ie/`)
3. Modify `blkdat.f` for new species list
4. Adjust `dgf.f`, `morts.f`, `htdbh.f` for regional equations
5. Create `FVSnewregion_sourceList.txt` in `bin/`
6. Add CMake build entry
7. Create test cases in `tests/FVSnewregion/`

### Debugging a Simulation

1. Add debug output in key routines (DGF, MORTS, etc.)
2. Check COMMON block values: print DBH, HT, BA, TPA
3. Enable compiler debug flags: `cmake -DCMAKE_BUILD_TYPE=Debug`
4. Use gdb (Linux) or debugger on Windows
5. Trace through GROWS cycle (DG calculation, mortality, etc.)

### Adding a New Module (e.g., Pest Effects)

1. Create directory: `mkdir pest/`
2. Create core routines: `pest/pest.f`, `pest/pestinit.f`
3. Create variant overrides: `pest/ak/pesteff.f`, etc.
4. Define COMMON block: `pest/common/PESTCOM.F77`
5. Integrate into FVS loop:
   * Add keyword processing in `base/algkey.f`
   * Call pest routine in `base/fvs.f` cycle
6. Build and test

---

## Appendix D: External Resources

* **FVS Wiki**: https://github.com/USDAForestService/ForestVegetationSimulator/wiki
* **FVS Official Website**: https://www.fs.usda.gov/fvs/index.shtml
* **Fortran 77 Reference**: https://www.cs.mtu.edu/~shene/COURSES/f77/
* **CMake Documentation**: https://cmake.org/documentation/
* **gfortran Compiler**: https://gcc.gnu.org/fortran/

---

**Document Version**: 1.0
**Last Updated**: March 2026
**Maintainer**: FVS Development Team (FMSC)

**Feedback and Questions**: sm.fs.fvs-support@usda.gov
