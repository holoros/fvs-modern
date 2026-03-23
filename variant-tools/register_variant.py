#!/usr/bin/env python3
"""
Register a new FVS variant in the build system.

This script validates a variant's source files and generates the necessary
CMake configuration to build it as part of the FVS project.

Usage:
    python register_variant.py VARIANT_CODE [SOURCE_DIRECTORY]

Examples:
    python register_variant.py xx my_variant_xx/
    python register_variant.py xx .    (if already in variant directory)

The script will:
    1. Validate all required Fortran files exist
    2. Check Fortran free-form format
    3. Generate CMakeLists.txt snippet
    4. Print instructions for integrating into main build

"""

import sys
import os
import re
from pathlib import Path


class VariantValidator:
    """Validate and register an FVS variant."""

    REQUIRED_FILES = [
        'crown.f90',
        'dgf.f90',
        'morts.f90',
        'htdbh.f90',
        'blkdat.f90',
        'sitset.f90',
        'PRGPRM.F90',
    ]

    OPTIONAL_FILES = [
        'README.md',
        'COEFFICIENTS.txt',
        'SPECIES_LIST.txt',
    ]

    def __init__(self, variant_code, source_dir=None):
        """Initialize validator.

        Args:
            variant_code: Two-letter variant code (e.g., 'xx')
            source_dir: Path to variant source directory
        """
        self.variant_code = variant_code.lower()
        if not source_dir:
            source_dir = f'variant_{variant_code}' if variant_code else '.'
        self.source_dir = Path(source_dir).resolve()
        self.issues = []
        self.warnings = []

    def validate_code_format(self, filepath):
        """Check if file appears to be valid Fortran 90 free-form.

        Args:
            filepath: Path to Fortran file

        Returns:
            bool: True if appears valid
        """
        try:
            with open(filepath, 'r') as f:
                content = f.read(1000)  # Check first 1000 chars

            # Checks for free-form Fortran 90
            if re.search(r'^[C*!]', content, re.MULTILINE):
                self.warnings.append(
                    f"  {filepath.name}: Contains fixed-form comments "
                    "(C* or ! in column 1). Should use ! for free-form.")
                return False

            # Check for module or subroutine declaration
            if not re.search(r'\b(module|subroutine|program|function)\b', content, re.IGNORECASE):
                self.warnings.append(
                    f"  {filepath.name}: Doesn't seem to contain module, subroutine, "
                    "program, or function declaration")
                return False

            return True
        except UnicodeDecodeError:
            self.issues.append(f"  {filepath.name}: Not a valid text file")
            return False

    def validate_structure(self):
        """Validate variant directory structure.

        Returns:
            bool: True if structure is valid
        """
        print(f"\nValidating variant: {self.variant_code}")
        print(f"Source directory: {self.source_dir}")
        print()

        if not self.source_dir.exists():
            self.issues.append(f"Source directory does not exist: {self.source_dir}")
            return False

        if not self.source_dir.is_dir():
            self.issues.append(f"Path is not a directory: {self.source_dir}")
            return False

        # Check required files
        print("Checking required files:")
        missing_files = []
        for filename in self.REQUIRED_FILES:
            filepath = self.source_dir / filename
            if filepath.exists():
                print(f"  ✓ {filename}")
                self.validate_code_format(filepath)
            else:
                print(f"  ✗ {filename} - MISSING")
                missing_files.append(filename)

        if missing_files:
            self.issues.append(f"Missing required files: {', '.join(missing_files)}")

        # Check optional files
        print("\nChecking optional files:")
        for filename in self.OPTIONAL_FILES:
            filepath = self.source_dir / filename
            if filepath.exists():
                print(f"  ✓ {filename}")
            else:
                print(f"  - {filename} (optional)")

        return len(self.issues) == 0

    def generate_cmake_snippet(self):
        """Generate CMakeLists.txt snippet for this variant.

        Returns:
            str: CMake configuration code
        """
        var_code_upper = self.variant_code.upper()
        var_code = self.variant_code
        cmake_code = f"""
# ============================================================================
# Variant {var_code_upper}: [Region Name]
# ============================================================================
# TODO: Update description with your variant name and region

set({var_code_upper}_SOURCES
  {var_code}/crown.f90
  {var_code}/dgf.f90
  {var_code}/morts.f90
  {var_code}/htdbh.f90
  {var_code}/blkdat.f90
  {var_code}/sitset.f90
  {var_code}/PRGPRM.F90
)

# Add variant sources to library
target_sources(fvs_variants PRIVATE ${{{var_code_upper}_SOURCES}})

# Optional: Add variant-specific compile flags if needed
# set_source_files_properties(${{{var_code_upper}_SOURCES}}
#   PROPERTIES COMPILE_FLAGS "-O2 -march=native"
# )

# Optional: Add variant to registry if using dynamic variant loading
# list(APPEND FVS_VARIANTS "{var_code}")
"""
        return cmake_code.strip()

    def generate_cmake_include(self):
        """Generate a complete CMakeLists.txt include file for the variant.

        Returns:
            str: Complete CMakeLists.txt content
        """
        var_code_upper = self.variant_code.upper()
        var_code = self.variant_code
        # Use string formatting with % to avoid f-string curly brace issues
        cmake_content = """# CMakeLists.txt for Variant %s
#
# This file can be included in the main FVS CMakeLists.txt with:
#   include(variants/%s/CMakeLists.txt)
#
# Or the source files can be added directly to the main FVS library.

set(%s_SOURCES
  crown.f90
  dgf.f90
  morts.f90
  htdbh.f90
  blkdat.f90
  sitset.f90
  PRGPRM.F90
)

# Make paths absolute
foreach(source $${%s_SOURCES})
  list(APPEND %s_ABS_SOURCES
    $${CMAKE_CURRENT_SOURCE_DIR}/$${source})
endforeach()

# Export for parent CMakeLists.txt
set(%s_SOURCES $${%s_ABS_SOURCES} PARENT_SCOPE)
""" % (var_code_upper, var_code, var_code_upper, var_code_upper,
       var_code_upper, var_code_upper, var_code_upper)
        return cmake_content.strip()

    def generate_variant_info(self):
        """Generate a variant information file for the registry.

        Returns:
            str: Variant info in JSON-like format
        """
        info = f"""{{
  "code": "{self.variant_code}",
  "name": "TODO: Region Name",
  "description": "TODO: Brief description of geographic and ecological scope",
  "region": "TODO: States/provinces included",
  "species_count": 108,
  "calibration_date": "TODO: YYYY-MM-DD",
  "data_sources": {{
    "inventory": "TODO: FIA or other source",
    "growth": "TODO: Data for fitting models",
    "mortality": "TODO: Data for mortality model"
  }},
  "contacts": [
    {{
      "name": "TODO: Your Name",
      "email": "TODO: your.email@example.com",
      "role": "Principal Investigator"
    }}
  ],
  "references": [
    "TODO: Cite calibration studies and regional guides"
  ],
  "validation": {{
    "independent_test_rmse": "TODO: RMSE on independent test data",
    "crown_r2": "TODO: Crown model R-squared",
    "growth_r2": "TODO: Diameter growth model R-squared",
    "mortality_auc": "TODO: Mortality model AUC"
  }}
}}"""
        return info

    def print_report(self):
        """Print validation report."""
        print("\n" + "="*70)
        print("VALIDATION REPORT")
        print("="*70)

        if not self.issues and not self.warnings:
            print("\n✓ All checks passed!")
            return True

        if self.issues:
            print("\n⚠ ERRORS (must fix):")
            for issue in self.issues:
                print(f"  {issue}")

        if self.warnings:
            print("\n⚠ WARNINGS (should review):")
            for warning in self.warnings:
                print(f"  {warning}")

        return len(self.issues) == 0

    def print_integration_instructions(self):
        """Print instructions for integrating variant into FVS build."""
        print("\n" + "="*70)
        print("INTEGRATION INSTRUCTIONS")
        print("="*70)

        print(f"""
1. Copy variant files to FVS source tree:
   mkdir -p fvs-modern/variants/{self.variant_code}
   cp {self.source_dir}/* fvs-modern/variants/{self.variant_code}/

2. Add to main CMakeLists.txt (in fvs-modern/CMakeLists.txt):

   Add this to the variant sources section:

{self.generate_cmake_snippet()}

3. Build FVS with the new variant:
   cd fvs-modern/build
   cmake ..
   make -j 4

4. Test the variant:
   ./fvs_simulation --variant {self.variant_code} --input test_inventory.txt

5. Document the variant:
   - Complete README.md in variant directory
   - Document model equations and coefficients
   - Provide validation statistics
   - List data sources

6. Submit to FVS project:
   - Email to: fvs-feedback@fs.fed.us
   - Include all source files and documentation
   - Provide contact information for maintenance
""")

    def print_cmake_files(self):
        """Print generated CMake configuration files for review."""
        print("\n" + "="*70)
        print("GENERATED CMAKE CONFIGURATION")
        print("="*70)

        print("\nSnippet for inclusion in main CMakeLists.txt:")
        print("-" * 70)
        print(self.generate_cmake_snippet())

        print("\n\nGenerated CMakeLists.txt (for variant/{self.variant_code}/):")
        print("-" * 70)
        print(self.generate_cmake_include())

        print("\n\nVariant registry entry (metadata):")
        print("-" * 70)
        print(self.generate_variant_info())


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print(__doc__)
        print("\nUsage:")
        print("  python register_variant.py VARIANT_CODE [SOURCE_DIRECTORY]")
        print("\nExample:")
        print("  python register_variant.py xx ./my_variant_xx/")
        sys.exit(1)

    variant_code = sys.argv[1]
    source_dir = sys.argv[2] if len(sys.argv) > 2 else None

    # Validate code format
    if len(variant_code) != 2 or not variant_code.isalpha():
        print(f"Error: Variant code must be 2 letters (got '{variant_code}')")
        sys.exit(1)

    # Create validator and run checks
    validator = VariantValidator(variant_code, source_dir)

    if not validator.validate_structure():
        validator.print_report()
        sys.exit(1)

    # Print results
    if validator.print_report():
        validator.print_cmake_files()
        validator.print_integration_instructions()
        print("\n" + "="*70)
        print("VARIANT REGISTRATION COMPLETE")
        print("="*70)
        print(f"\nVariant '{variant_code}' is ready for integration.")
        print("Follow the integration instructions above to add to FVS build.\n")
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
