# Generated Fortran DATA Statements

This directory contains auto-generated Fortran 90 DATA statements created from the JSON configuration files in the parent `config/` directory.

## Purpose

These files demonstrate the "reverse" round-trip of parameter extraction:

1. **Forward (Extract)**: `extract_parameters.py` reads Fortran source files and extracts DATA statements into JSON config files
2. **Reverse (Generate)**: `generate_fortran.py` reads JSON config files and generates Fortran 90 DATA statements

This enables:
- Human-readable parameter editing in JSON (version-controlled, diff-able)
- Regeneration of Fortran source from modified parameters
- Foundation for runtime parameter loading (replacing hardcoded DATA)

## Generated Files

- `ne_generated.f90` - Northeastern US variant (108 species, 29 arrays)
- `ie_generated.f90` - Inland Empire variant (23 species, 61 arrays)
- `sn_generated.f90` - Southern variant (90 species, 78 arrays)
- `VERIFICATION_REPORT.txt` - Round-trip verification results

## Verification Results

All variants successfully passed round-trip verification:

```
NE: PASS (29/29 arrays)
IE: PASS (61/61 arrays)
SN: PASS (78/78 arrays)
```

This confirms that:
1. All original Fortran DATA values are preserved in JSON configs
2. Generated Fortran statements match the original values exactly (within floating-point tolerance)
3. The conversion is lossless and reversible

## Features

The generated Fortran files use:

- **Free-form Fortran 90** - No fixed column restrictions
- **Repetition syntax** - Consecutive identical values use Fortran repetition (e.g., `27*0.9` instead of 27 separate values)
- **72-character line width** - Respects Fortran convention with `&` continuation markers
- **Smart formatting** - Decimal places adjusted to match original precision
- **Category comments** - Each array includes its category for documentation

## Example Output

```fortran
DATA  BKRAT/2*0.9349,0.956,4*0.9324,2*0.92,0.89,0.964,4*0.95,2*0.934 &
            8*0.964,0.95,3*0.92,5*0.948,5*0.9,0.95,13*0.9,0.94,5*0.91 &
            4*0.9,3*0.88,42*0.9/  ! bark_ratio
```

## Integration Path

These generated files can be integrated into the source tree as:

1. Reference files for validation
2. Templates for manual parameter updates
3. Basis for runtime loading infrastructure
4. Documentation of parameter origins and categories

## Notes

- Non-numeric arrays (strings, format specifications) are excluded
- Values are extracted only from the "files_parsed" list in each JSON config to avoid duplicate definitions
- The generation script is idempotent - running it multiple times produces identical output
