# FVS Fortran Modernization Effort - Deliverables

## Quick Start

This directory contains two major deliverables for the FVS Forest Vegetation Simulator Fortran modernization effort:

1. **GOTO Refactoring Analysis** - Identifies all control flow issues
2. **COMMON Block Migration Guide** - Strategy for modernizing global state

## Files Overview

### Reports (Analysis Results)
* **goto_catalog_report.md** - GOTO patterns analysis (13,418 found)
  * Directory-by-directory breakdown
  * Top 20 files by refactoring priority
  * Before/after code examples for each GOTO type
  * Modern Fortran replacements

* **common_block_inventory.md** - COMMON block dependency analysis
  * All 104 COMMON blocks cataloged with usage counts
  * Top 20 most-used blocks (PRGPRM: 1,675 files, CONTRL: 1,193 files)
  * Dependency matrix for top 15 blocks
  * Migration order recommendations

* **common_block_dependencies.md** - Detailed file-to-block mapping
  * Reference for tracing which files depend on which blocks

### Guides
* **common_to_module_guide.md** - Comprehensive migration strategy (800+ lines)
  * Current state analysis of key COMMON blocks
  * 3-phase migration approach (12 weeks estimated)
  * Before/after code examples for COEFFS, PLOT, and ARRAYS migrations
  * Step-by-step procedure with hybrid approach
  * Handling of variant-specific blocks (20 variants)
  * Testing strategy and timeline

### Scripts (Executable Analysis Tools)
* **goto_catalog.py** - Identifies all GOTO patterns in Fortran code
  * Usage: `python3 goto_catalog.py`
  * Detects simple GOTO, computed GOTO, assigned GOTO, arithmetic IF
  * Generates comprehensive report
  * Runtime: ~30 seconds for full FVS codebase

* **common_block_inventory.py** - Analyzes COMMON block dependencies
  * Usage: `python3 common_block_inventory.py`
  * Lists all INCLUDE statements and their relationships
  * Generates dependency matrix
  * Creates migration order recommendations
  * Runtime: ~30 seconds for full FVS codebase

## Key Findings

### GOTO Analysis
* **Total patterns found:** 13,418
* **Simple GOTO (GO TO nnn):** 13,310 (99.2%)
* **Computed GOTO (GO TO(...), expr):** 104 (0.8%)
* **Arithmetic IF (IF(expr) n1,n2,n3):** 4 (0.03%)
* **Assigned GOTO:** 0

**Most GOTO-dense files:**
1. base/iqrsrt.f - 22.08% (17 GOTOs in 77 lines)
2. base/opbisr.f - 17.50% (7 GOTOs in 40 lines)
3. lpmpb/ptssc.f - 17.37% (29 GOTOs in 167 lines)

### COMMON Block Analysis
* **Total unique blocks:** 104
* **Files using INCLUDE:** 1,806 source files
* **Most-used blocks:**
  1. PRGPRM.F77 - 1,675 files (variant-specific program parameters)
  2. CONTRL.F77 - 1,193 files (control parameters)
  3. PLOT.F77 - 919 files (stand attributes)
  4. ARRAYS.F77 - 717 files (tree attributes)

**Key blocks for Phase 1 migration:**
* COEFFS.F77 (127 lines, 352 users) - Recommended first
* PLOT.F77 (191 lines, 919 users)
* ARRAYS.F77 (263 lines, 717 users)
* CONTRL.F77 (348 lines, 1,193 users)

## How to Use

### For GOTO Refactoring
1. Review **goto_catalog_report.md** for overview of issues
2. Focus on top 20 files by GOTO density
3. Use provided before/after code examples for each GOTO type
4. Start with simple GOTO patterns (99.2% of issues)

### For COMMON Block Migration
1. Read **common_to_module_guide.md** for complete strategy
2. Review **common_block_inventory.md** for dependency analysis
3. Follow the 3-phase approach in the guide
4. Use examples for COEFFS, PLOT, ARRAYS as templates
5. Plan based on 12-week estimated timeline with 1-2 developers

### Regenerating Analysis
Both Python scripts can be run at any time to regenerate reports:
```bash
cd scripts
python3 goto_catalog.py      # Regenerates goto_catalog_report.md
python3 common_block_inventory.py  # Regenerates inventory and dependencies
```

## FVS Codebase Scope

* **Total files analyzed:** 2,193 Fortran files
* **Total size:** ~500,000+ lines of code
* **Variants:** ~20 (AK, NC, EM, etc., each with variant-specific COMMON blocks)
* **Analysis location:** /path/to/ForestVegetationSimulator-main/

## Timeline Estimate (From Guide)

* **Phase 1 (Weeks 1-4):** Core modules - COEFFS, PLOT, ARRAYS
* **Phase 2 (Weeks 5-8):** Control blocks - CONTRL and support modules
* **Phase 3 (Weeks 9-12):** Variant blocks and integration

**Total effort:** 12 weeks with 1 developer, or 8 weeks with 2 developers

## Next Steps

1. **Immediate:** Review SUMMARY.md for overview and findings
2. **Planning:** Study common_to_module_guide.md for migration strategy
3. **Analysis:** Review specific reports for GOTO patterns and block dependencies
4. **Implementation:** Follow the phased approach starting with Phase 1 blocks

## Files at a Glance

```
fvs-modernization/
├── README.md                           # This file
├── SUMMARY.md                          # Executive summary of all deliverables
├── scripts/
│   ├── goto_catalog.py                 # GOTO analysis tool
│   └── common_block_inventory.py       # Dependency analysis tool
├── reports/
│   ├── goto_catalog_report.md          # GOTO findings (12 KB)
│   ├── common_block_inventory.md       # Block usage statistics (19 KB)
│   └── common_block_dependencies.md    # File dependencies (200 KB)
└── guides/
    └── common_to_module_guide.md       # Migration strategy (33 KB)
```

## Contact & Documentation

* **GOTO Report:** Comprehensive analysis with 13,418 findings
* **Migration Guide:** 800+ lines covering strategy, examples, and timeline
* **Scripts:** Standalone Python tools for independent analysis
* **Reports:** Markdown format for easy viewing and sharing

---

Generated: March 22, 2026

