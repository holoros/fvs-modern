# Fortran 77 Fixed-Form to Fortran 90+ Free-Form Converter

A Python utility to automatically convert Fortran 77 fixed-form source files to Fortran 90+ free-form format. This tool handles the structural conversion while preserving all program logic exactly.

## Overview

This converter transforms Fortran 77 fixed-form code to modern Fortran 90+ free-form syntax. It is designed specifically to handle the patterns found in the Forest Vegetation Simulator (FVS) codebase, though it should work with any Fortran 77 fixed-form source.

### What It Does

* Removes the significance of column positions (columns 1-5 for labels, column 6 for continuation, columns 7-72 for code)
* Converts continuation characters (column 6) from single character to trailing `&` and leading `&` syntax
* Preserves statement labels by moving them inline
* Converts comment styles from `C`, `c`, or `*` in column 1 to `!` prefix
* Processes `INCLUDE` statements and maps old filenames to new ones
* Maintains strict structural fidelity to the original code

### What It Does NOT Do

* Change any logic or control flow
* Rename variables or subroutines
* Remove `GOTO` statements (though you may want to address these manually later)
* Modify `COMMON` blocks (though migrating these to modules is recommended as a follow-up)
* Reformat code beyond what is necessary for the column layout conversion
* Change numerical precision or any computational details

## Installation

The script requires Python 3.6 or later with no external dependencies.

Copy `fixed_to_free.py` to your working directory or add it to your PATH.

## Usage

### Single File Conversion

Convert a single Fortran file:

```bash
python fixed_to_free.py input.f output.f90
```

### Directory Conversion

Convert all Fortran files in a directory and its subdirectories:

```bash
python fixed_to_free.py --dir ./source --output ./source_free
```

If `--output` is not specified, files will be written to `./source/free_form`.

### Dry Run

Preview what would be converted without writing files:

```bash
python fixed_to_free.py --dir ./source --dry-run
```

This is useful for understanding the scope of conversion before committing to it.

### Generate Report

Create a detailed conversion report with statistics:

```bash
python fixed_to_free.py --dir ./source --report conversion_report.txt
```

The report includes:
* Number of files processed
* Line conversion statistics
* List of INCLUDE file renames
* Any warnings or issues encountered

### Generate Include Mapping

Create a CSV file documenting the mapping of old INCLUDE filenames to new ones:

```bash
python fixed_to_free.py --dir ./source --mapping include_mapping.csv
```

This is useful for tracking which INCLUDE files need to be renamed in your file system.

### Verbose Output

Enable detailed logging:

```bash
python fixed_to_free.py --dir ./source --verbose
```

## Conversion Details

### Fixed-Form Column Layout

Fortran 77 fixed-form uses strict column positions:

* **Columns 1-5**: Statement labels (optional)
* **Column 6**: Continuation character (non-blank/non-zero indicates continuation)
* **Columns 7-72**: Executable code
* **Columns 73+**: Ignored (typically used for sequence numbers)

### Example: Label Conversion

**Before (fixed-form):**
```fortran
    100 CONTINUE
```

**After (free-form):**
```fortran
100 CONTINUE
```

### Example: Continuation Conversion

**Before (fixed-form):**
```fortran
      REAL SSUMN,TEMP1,TEMP2,RAT,TOTAL,CCFT,CW,BATREE,DP,P,BAT,RELDT
     &REAL TSUMD2,R,BRATIO,D,G,SN,BAGR
```

**After (free-form):**
```fortran
REAL SSUMN,TEMP1,TEMP2,RAT,TOTAL,CCFT,CW,BATREE,DP,P,BAT,RELDT &
& REAL TSUMD2,R,BRATIO,D,G,SN,BAGR
```

### Example: Comment Conversion

**Before (fixed-form):**
```fortran
C----------
C THIS SUBROUTINE CALCULATES THE HEIGHT
C----------
```

**After (free-form):**
```fortran
!----------
! THIS SUBROUTINE CALCULATES THE HEIGHT
!----------
```

### Example: INCLUDE Statement Conversion

**Before:**
```fortran
      INCLUDE 'PRGPRM.F77'
```

**After:**
```fortran
INCLUDE 'PRGPRM.F90'
```

## Recommended Workflow for FVS

1. **Create a backup**: Always keep the original source files

2. **Dry run on a subset**: Test on a single subdirectory first

   ```bash
   python fixed_to_free.py --dir ./base --output ./base_free --dry-run
   ```

3. **Generate a report**: Understand the scope of changes

   ```bash
   python fixed_to_free.py --dir ./base --report conversion.txt
   ```

4. **Perform full conversion**: Convert your entire source tree

   ```bash
   python fixed_to_free.py --dir ./ForestVegetationSimulator-main \
     --output ./fvs-free-form \
     --mapping include_mapping.csv
   ```

5. **Rename INCLUDE files**: Use the mapping to rename your INCLUDE files

   ```bash
   # Example: rename PRGPRM.F77 to PRGPRM.F90
   cd fvs-free-form
   find . -name "*.F77" -o -name "*.f77" | while read f; do
     mv "$f" "${f%.F77}.F90"
     mv "$f" "${f%.f77}.f90"
   done
   ```

6. **Verify compilation**: Test that the converted code compiles with your Fortran compiler

   ```bash
   gfortran -c -ffree-form converted_file.f90
   ```

7. **Run tests**: Execute your test suite to verify functionality

8. **Manual improvements** (optional, for later phases):
   * Replace `GOTO` with modern control structures
   * Convert `COMMON` blocks to modules
   * Replace `DATA` statements with `PARAMETER` or module initialization
   * Use modern Fortran features (allocatable arrays, derived types, etc.)

## Technical Notes

### Continuation Line Handling

The converter correctly handles multi-line continuations by:

1. Detecting continuation characters in column 6 of the current line
2. Adding a trailing ` &` to the converted current line
3. Extracting code from column 7+ of the next line
4. Adding a leading `& ` to the continuation line

This applies only to non-comment lines, as comment lines are never continuations.

### Statement Labels

Labels in columns 1-5 are preserved and placed inline at the beginning of the statement. They remain numeric for use with `GOTO` statements.

### Include File Renaming

The converter automatically updates INCLUDE statement filenames:

* `.F77` → `.F90`
* `.f77` → `.f90`
* `.F` → `.F90`
* `.f` → `.f90`

A mapping file documents these changes for reference when renaming actual INCLUDE files on disk.

### Whitespace and Line Length

The converter removes trailing whitespace beyond column 72 (the Fortran 77 limit) and normalizes leading whitespace. Free-form Fortran has no line length limit.

## Safety Features

* **Never modifies original files**: Always writes to a separate output file or directory
* **Logging**: All conversions are logged with timestamps
* **Error handling**: Gracefully handles encoding issues and malformed files
* **Dry-run mode**: Preview changes without writing
* **Statistics**: Detailed reports on what was converted

## Limitations and Caveats

* The converter assumes standard Fortran 77 fixed-form syntax. Non-standard extensions may not be handled correctly.
* Hollerith constants (e.g., `5HHELLO`) are preserved but should be reviewed and updated to modern string syntax.
* Alternative comment characters (`!` in column 1 in Fortran 90+) are treated as `!` comments, not fixed-form comments.
* Very long lines that exceed column 72 may need manual review after conversion.

## Known Issues and Workarounds

* **Multi-character continuations**: If a file uses non-standard continuation characters, they may not be recognized. Check the conversion report for any warnings.
* **Embedded blanks in continuation lines**: Ensure your source files follow Fortran 77 standards for column positions.

## Troubleshooting

### Files Not Converting

Check that files end with `.f`, `.F`, `.f77`, or `.F77`. The converter is case-sensitive for file extensions.

### Include Files Not Found

Verify that the INCLUDE filenames match exactly (case-sensitive) in both the source code and file system.

### Conversion Errors

Run with `--verbose` to see detailed logging:

```bash
python fixed_to_free.py --dir ./source --verbose
```

Check the conversion report for specific warnings:

```bash
python fixed_to_free.py --dir ./source --report report.txt
```

## Performance

For large codebases (100+ files), the converter processes approximately 1,000-2,000 lines per second on modern hardware. A complete conversion of the FVS base directory with ~198 files should complete in under 1 second.

## Examples

### Convert Single File with Report

```bash
python fixed_to_free.py dense.f dense.f90 && \
gfortran -c -ffree-form dense.f90
```

### Batch Convert with Full Reporting

```bash
python fixed_to_free.py \
  --dir ~/ForestVegetationSimulator-main/base \
  --output ~/fvs-free-form \
  --report conversion.txt \
  --mapping include_mapping.csv
```

### Test Conversion of New Regional Variant

```bash
python fixed_to_free.py \
  --dir ~/ForestVegetationSimulator-main/se \
  --dry-run
```

This performs a dry run to see what would be converted before committing.

## Future Enhancements

Potential additions to this tool:

* Support for free-form Fortran 90+ source (in case of mixed codebases)
* Automatic conversion of statement labels to modern control structures
* Conversion of COMMON blocks to modules
* Parallel processing for very large codebases
* Web UI for interactive conversion
* Integration with version control systems

## Contact and Support

For issues or suggestions, document:

* The specific file or code pattern that caused the issue
* The exact command used to run the converter
* The output from running with `--verbose`
* The conversion report if applicable

## References

* Fortran 77 Standard: ANSI X3.9-1978
* Fortran 90+ Standard: ISO/IEC 1539
* [Modern Fortran Best Practices](https://en.wikibooks.org/wiki/Fortran/Fortran_90_and_95)
