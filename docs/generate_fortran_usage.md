# generate_fortran.py Usage Guide

## Overview

`generate_fortran.py` is a round-trip tooling script that regenerates Fortran 90 DATA statements from JSON configuration files. This enables a modern parameter management workflow.

## Quick Start

```bash
cd /path/to/Documents/Claude/fvs-modern
python3 modernization/generate_fortran.py
```

This will:
1. Read all JSON configs from `config/`
2. Generate Fortran 90 files in `config/fortran/`
3. Verify the round-trip for NE, IE, and SN variants
4. Report results to console and `config/fortran/VERIFICATION_REPORT.txt`

## Workflow

### 1. Edit Parameters in JSON (Human-Readable)

```json
{
  "variant": "ne",
  "categories": {
    "bark_ratio": {
      "BKRAT": [0.9349, 0.9349, 0.956, 0.9324, ...]
    }
  }
}
```

**Advantages:**
- Easy to review and understand
- Version control friendly (shows exact changes)
- Can use JSON tools for batch processing
- No Fortran compilation needed to validate

### 2. Generate Fortran Source

```bash
python3 modernization/generate_fortran.py
```

This produces `config/fortran/ne_generated.f90`:

```fortran
DATA  BKRAT/2*0.9349,0.956,4*0.9324,2*0.92,0.89,0.964,4*0.95,2*0.934 &
            8*0.964,0.95,3*0.92,5*0.948,5*0.9,0.95,13*0.9,0.94,5*0.91 &
            4*0.9,3*0.88,42*0.9/  ! bark_ratio
```

### 3. Integrate into Build

Copy generated files into source tree or use as reference for manual updates.

### 4. Verify Round-Trip

Run the script to verify values match:

```
NE: PASS (29/29 arrays)
IE: PASS (61/61 arrays)
SN: PASS (78/78 arrays)
```

## Script Architecture

### Classes

#### `FortranDataGenerator`
Main class handling conversion and verification.

**Key Methods:**
- `load_config(variant)` - Load JSON config
- `generate_fortran_file(variant, config)` - Generate Fortran content
- `detect_repetitions(values)` - Find consecutive repeats
- `verify_round_trip(variant, config)` - Validate against originals
- `extract_original_fortran_values()` - Read original source files

### Key Features

#### 1. Automatic Repetition Detection
```python
Input:  [0.9324, 0.9324, 0.9324, 0.9324]
Output: 4*0.9324
```

#### 2. Line Width Management
Respects 72-character Fortran convention:
```fortran
DATA  ARRAY/val1,val2,val3,val4,val5,val6,val7,val8, &
            val9,val10,val11,...  ! comment
```

#### 3. Precision Handling
Automatically detects and preserves decimal places:
```python
0.0008829  → 0.0009 (4 decimals for small values)
0.1038458  → 0.1038 (4 decimals)
1.0        → 1.0    (1 decimal for integers)
```

#### 4. File-Aware Extraction
Uses `files_parsed` from config to avoid conflicts:
- Some variants have multiple DATA statements with same name
- Script respects the order in `files_parsed` list
- Later files override earlier ones (correct precedence)

## Output Structure

### Generated File Format

```fortran
! AUTO-GENERATED FILE
! Variant: ne (Northeastern US)
! MAXSP: 108
! Generated from JSON config
! Files parsed: blkdat.f90, crown.f90, dgf.f90, ...
!
! This file contains DATA statements for species-specific parameters.
! Do NOT edit directly - regenerate from config/.json files instead.
!

! === Category: bark_ratio ===

DATA  BKRAT/.../  ! bark_ratio

! === Category: growth ===

DATA  B1/.../  ! growth
DATA  B2/.../  ! growth

...
```

### Files Generated

- `ne_generated.f90` - Northeastern US (108 species, 29 arrays)
- `ie_generated.f90` - Inland Empire (23 species, 61 arrays)
- `sn_generated.f90` - Southern (90 species, 78 arrays)
- `VERIFICATION_REPORT.txt` - Test results

## Verification Details

### What Gets Verified

1. **Value Count** - Original and generated must have same number of values
2. **Value Equality** - Each value must match original
3. **Type Consistency** - Integer vs float format preserved
4. **Precision** - Floating-point values compared with 1e-6 tolerance

### Comparison Process

```python
# For each array in each variant:
1. Extract original values from src-converted/{variant}/ files
2. Get generated values from config JSON
3. Compare value-by-value
4. Report any discrepancies
```

### Tolerance Settings

```python
# Integer values
if both are integers:
    exact match required

# Floating-point values  
if abs(original - generated) < 1e-6:
    PASS
else:
    FAIL
```

## Common Tasks

### Generate for All Variants

```bash
python3 modernization/generate_fortran.py
```

Processes: NE, IE, SN by default (configurable in `main()`)

### Verify Only (Without Regenerating)

Add to script:
```python
generator.verify_round_trip(variant, config)
```

### Check Specific Array

```bash
grep -A 5 "DATA  BKRAT" config/fortran/ne_generated.f90
```

### View Verification Details

```bash
cat config/fortran/VERIFICATION_REPORT.txt
```

## Technical Notes

### Dependencies
- Python 3 (standard library only)
- `re`, `json`, `pathlib`, `logging`, `dataclasses`

### Performance
- ~2-3 seconds for all three test variants
- Scales linearly with number of arrays
- No external dependencies required

### Idempotency
- Same input always produces identical output
- Checksums stable between runs
- Safe to run in build pipeline

### Error Handling
- Graceful handling of missing files
- Descriptive logging at each step
- Detailed mismatch reporting

## Integration Examples

### As Part of Build System

```makefile
fortran-from-json:
	cd fvs-modern && python3 modernization/generate_fortran.py
	cp config/fortran/*_generated.f90 src/

verify-parameters:
	python3 modernization/generate_fortran.py
```

### For Parameter Updates

```bash
# 1. Edit JSON (tracked in git)
git checkout config/ne.json
# ... edit values ...
git add config/ne.json
git commit -m "Update NE bark ratio parameters"

# 2. Regenerate Fortran
python3 modernization/generate_fortran.py

# 3. Verify and integrate
git add config/fortran/ne_generated.f90
```

### For Validation

```bash
# Before deployment, verify all parameters
python3 modernization/generate_fortran.py

# Check output
if [ $? -eq 0 ]; then
    echo "Parameter round-trip verification passed"
else
    echo "Parameter mismatch detected"
    exit 1
fi
```

## Troubleshooting

### Script Not Finding Variants

```python
# Check if JSON files exist
ls config/*.json

# Check variant name in script
grep "test_variants = " modernization/generate_fortran.py
```

### Verification Failures

1. Check that `files_parsed` in JSON matches actual files
2. Verify source files exist in `src-converted/{variant}/`
3. Review `VERIFICATION_REPORT.txt` for specific mismatches

### Output Not Generated

1. Verify `config/` and `src-converted/` directories exist
2. Check file permissions
3. Review console output for error messages

## Future Enhancements

Possible improvements:

1. **Selective Generation** - Generate specific variants only
2. **Format Customization** - Configurable line width, decimal places
3. **Diff Mode** - Show what would change before committing
4. **Configuration** - External config file for behavior
5. **Integration** - Direct Fortran module generation
6. **Testing** - Unit tests for each component

## References

- `extract_parameters.py` - Reverse direction (Fortran → JSON)
- `config/` - Input JSON configuration files
- `src-converted/` - Original Fortran source files
- `VERIFICATION_REPORT.txt` - Results of round-trip verification
