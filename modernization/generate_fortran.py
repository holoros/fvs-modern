#!/usr/bin/env python3
"""
Generate Fortran 90 DATA statements from JSON config files.

This is the "reverse" of extract_parameters.py: it takes JSON config files
containing extracted parameter values and regenerates properly formatted
Fortran 90 DATA statements.

Functionality:
1. Reads variant JSON config files
2. Generates formatted Fortran 90 DATA statements with:
   - Free form Fortran 90 (no column restrictions)
   - Repetition syntax optimization (e.g., 4*0.9324)
   - 72-character line width with & continuation
   - Appropriate decimal formatting for floats/ints
3. Generates output files in /fortran/ directory
4. Performs round-trip verification against original src-converted/ files
5. Generates comparison reports for verification
"""

import re
import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Any, Optional
import logging
from dataclasses import dataclass
from collections import defaultdict

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


@dataclass
class VerificationResult:
    """Results of round-trip verification for a single array."""
    variant: str
    category: str
    array_name: str
    success: bool
    original_values: List[float]
    generated_values: List[float]
    discrepancies: List[str]
    value_count: int


class FortranDataGenerator:
    """Generate Fortran 90 DATA statements from JSON config."""

    def __init__(self, config_dir: str, output_dir: str, src_dir: str):
        """Initialize generator.

        Args:
            config_dir: Directory containing JSON config files
            output_dir: Directory to write generated .f90 files
            src_dir: Directory containing original Fortran src-converted/
        """
        self.config_dir = Path(config_dir)
        self.output_dir = Path(output_dir)
        self.src_dir = Path(src_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # For verification
        self.verification_results: Dict[str, List[VerificationResult]] = defaultdict(list)

    def load_config(self, variant: str) -> Optional[Dict[str, Any]]:
        """Load JSON config for a variant."""
        config_path = self.config_dir / f"{variant}.json"
        if not config_path.exists():
            logger.error(f"Config not found: {config_path}")
            return None

        try:
            with open(config_path, 'r') as f:
                return json.load(f)
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse {config_path}: {e}")
            return None

    def detect_value_type(self, values: List[Any]) -> Optional[str]:
        """Detect if values are integers or floats. Returns None for non-numeric."""
        if not values:
            return 'float'

        # Check if all values are numeric
        for v in values:
            if isinstance(v, str):
                return None  # String array, skip
            elif isinstance(v, (int, float)):
                pass
            else:
                return None

        # Check if all are effectively integers
        for v in values:
            if isinstance(v, float):
                if v != int(v):
                    return 'float'
            elif isinstance(v, int):
                pass

        return 'int'

    def detect_repetitions(self, values: List[float]) -> List[Tuple[Any, int]]:
        """Detect consecutive repeated values and return (value, count) pairs.

        Returns list of (value, repeat_count) tuples.
        """
        if not values:
            return []

        result = []
        current_val = values[0]
        count = 1

        for i in range(1, len(values)):
            # For float comparison, use tolerance
            if self._values_equal(values[i], current_val):
                count += 1
            else:
                result.append((current_val, count))
                current_val = values[i]
                count = 1

        # Don't forget the last group
        result.append((current_val, count))
        return result

    def _values_equal(self, a: float, b: float, tol: float = 1e-10) -> bool:
        """Compare two float values with tolerance."""
        if isinstance(a, int) and isinstance(b, int):
            return a == b
        return abs(float(a) - float(b)) < tol

    def format_value(self, value: Any, value_type: str, decimal_places: int = 4) -> str:
        """Format a single value for Fortran output."""
        if value_type == 'int':
            return str(int(value))
        else:
            # Float: use decimal places, but strip trailing zeros after decimal
            if isinstance(value, int):
                return f"{value}.0"

            # Format with specified decimal places
            formatted = f"{float(value):.{decimal_places}f}"

            # Strip trailing zeros but keep at least one decimal place
            if '.' in formatted:
                formatted = formatted.rstrip('0')
                if formatted.endswith('.'):
                    formatted += '0'

            return formatted

    def generate_data_statement(self, array_name: str, values: List[Any],
                               category: str = '', max_line_width: int = 72) -> str:
        """Generate a Fortran DATA statement with proper formatting and continuations.

        Args:
            array_name: Name of the array
            values: List of values
            category: Category/comment for documentation
            max_line_width: Maximum line width (Fortran convention: 72)

        Returns:
            Formatted DATA statement(s) as string, or empty string if non-numeric
        """
        if not values:
            return ""

        value_type = self.detect_value_type(values)
        if value_type is None:
            # Skip non-numeric arrays (strings, etc.)
            return ""

        repetitions = self.detect_repetitions(values)

        # Build the value string
        value_strs = []
        for val, count in repetitions:
            formatted_val = self.format_value(val, value_type)
            if count > 1:
                value_strs.append(f"{count}*{formatted_val}")
            else:
                value_strs.append(formatted_val)

        values_part = ",".join(value_strs)

        # Add comment about category if provided
        comment = ""
        if category:
            comment = f"  ! {category}"

        # Start the DATA statement
        base_statement = f"DATA  {array_name}/"
        continuation_indent = " " * len(base_statement)

        # Split values into lines respecting width limit
        lines = []
        current_line = base_statement

        for i, val_str in enumerate(value_strs):
            test_line = current_line
            if i > 0 or current_line != base_statement:
                test_line += ","
            test_line += val_str

            # Check if adding this value exceeds line width
            if len(test_line) > max_line_width and current_line != base_statement:
                # Start a new line with continuation
                lines.append(current_line + " &")
                current_line = continuation_indent + val_str
            else:
                if i > 0:
                    current_line += ","
                current_line += val_str

        # Close the statement
        current_line += "/"
        if comment and len(current_line) + len(comment) <= max_line_width:
            current_line += comment
            lines.append(current_line)
        else:
            lines.append(current_line)
            if comment:
                lines.append(comment)

        return "\n".join(lines)

    def generate_fortran_file(self, variant: str, config: Dict[str, Any]) -> str:
        """Generate complete .f90 file with all DATA statements for a variant."""
        lines = []

        # Header comment
        lines.append("! AUTO-GENERATED FILE")
        lines.append(f"! Variant: {variant} ({config.get('variant_name', 'Unknown')})")
        lines.append(f"! MAXSP: {config.get('maxsp', 'Unknown')}")
        lines.append(f"! Generated from JSON config")
        lines.append(f"! Files parsed: {', '.join(config.get('files_parsed', []))}")
        lines.append("!")
        lines.append("! This file contains DATA statements for species-specific parameters.")
        lines.append("! Do NOT edit directly - regenerate from config/.json files instead.")
        lines.append("!")
        lines.append("")

        # Generate DATA statements for each category
        categories = config.get('categories', {})
        for category_name, arrays in sorted(categories.items()):
            lines.append(f"! === Category: {category_name} ===")
            lines.append("")

            for array_name, values in sorted(arrays.items()):
                data_stmt = self.generate_data_statement(
                    array_name, values, category=category_name
                )
                if data_stmt:  # Skip empty statements (non-numeric arrays)
                    lines.append(data_stmt)
                    lines.append("")

        return "\n".join(lines)

    def write_fortran_file(self, variant: str, content: str) -> Path:
        """Write generated Fortran content to file."""
        output_file = self.output_dir / f"{variant}_generated.f90"
        with open(output_file, 'w') as f:
            f.write(content)
        logger.info(f"Wrote: {output_file}")
        return output_file

    # ==================== Verification Methods ====================

    def extract_original_fortran_values(self, variant: str, config: Dict[str, Any]) -> Dict[str, Dict[str, List[float]]]:
        """Extract original DATA statement values from src-converted/{variant}/ files.

        Uses the files_parsed list from the config to know which files to extract from.
        Returns dict: {category: {array_name: [values]}}
        """
        variant_dir = self.src_dir / variant
        if not variant_dir.exists():
            logger.warning(f"Source directory not found: {variant_dir}")
            return {}

        result = defaultdict(dict)

        # Get the list of files that were parsed for this variant
        files_to_parse = config.get('files_parsed', [])
        if not files_to_parse:
            logger.warning(f"No files_parsed listed in config for {variant}")
            return {}

        # Parse only the files that were originally parsed
        for filename in files_to_parse:
            f90_file = variant_dir / filename
            if not f90_file.exists():
                logger.debug(f"File not found: {f90_file}")
                continue

            try:
                content = self._read_fortran_file(f90_file)
                # Extract DATA statements
                data_dict = self._extract_data_statements_from_content(content)

                # Store extracted data
                for array_name, values in data_dict.items():
                    # Keep the last/newest occurrence (later files override)
                    result['original'][array_name] = values
            except Exception as e:
                logger.debug(f"Error reading {f90_file}: {e}")

        return dict(result)

    def _read_fortran_file(self, filepath: Path) -> str:
        """Read a Fortran file and handle line continuations."""
        with open(filepath, 'r') as f:
            lines = f.readlines()

        # Join continuation lines
        joined_lines = []
        i = 0
        while i < len(lines):
            line = lines[i]

            # Accumulate continued lines
            while i < len(lines) - 1 and (
                line.rstrip().endswith('&') or
                (i + 1 < len(lines) and lines[i + 1].lstrip().startswith('&'))
            ):
                line = line.rstrip()
                if line.endswith('&'):
                    line = line[:-1]
                i += 1
                next_line = lines[i].lstrip()
                if next_line.startswith('&'):
                    next_line = next_line[1:]
                line = line + ' ' + next_line.rstrip()

            joined_lines.append(line)
            i += 1

        return '\n'.join(joined_lines)

    def _extract_data_statements_from_content(self, content: str) -> Dict[str, List[float]]:
        """Extract all DATA statements from Fortran content."""
        data_dict = {}

        # Split by 'DATA' keyword
        sections = re.split(r'\bDATA\b', content, flags=re.IGNORECASE)

        for section in sections[1:]:
            # Remove comments
            section = re.sub(r'!.*?(?=\n|$)', '', section)

            # Find all name/values pairs: name / values /
            pattern = r'(\w+)\s*/([^/]+)/'
            matches = re.finditer(pattern, section)

            for match in matches:
                array_name = match.group(1)
                values_str = match.group(2)

                # Parse values
                values = self._parse_data_values(values_str)
                if values:
                    data_dict[array_name] = values

        return data_dict

    def _parse_data_values(self, values_str: str) -> List[float]:
        """Parse Fortran DATA values including repetition syntax."""
        values = []
        current_value = ''
        paren_depth = 0

        for char in values_str:
            if char == '(':
                paren_depth += 1
                current_value += char
            elif char == ')':
                paren_depth -= 1
                current_value += char
            elif char == ',' and paren_depth == 0:
                if current_value.strip():
                    parsed = self._parse_repeated_value(current_value.strip())
                    values.extend(parsed)
                current_value = ''
            else:
                current_value += char

        # Don't forget the last value
        if current_value.strip():
            parsed = self._parse_repeated_value(current_value.strip())
            values.extend(parsed)

        return values

    def _parse_repeated_value(self, expr: str) -> List[float]:
        """Parse Fortran repetition syntax like '4*.9324'."""
        expr = expr.strip()

        # Check for repetition syntax: n*value
        match = re.match(r'(\d+)\*(.*)', expr)
        if match:
            count = int(match.group(1))
            value_str = match.group(2).strip()
            try:
                value = float(value_str)
                return [value] * count
            except ValueError:
                return []

        # Try to parse as single value
        try:
            return [float(expr)]
        except ValueError:
            return []

    def verify_round_trip(self, variant: str, config: Dict[str, Any]) -> bool:
        """Verify that generated DATA statements match the original values.

        Returns True if all values match (within tolerance), False otherwise.
        """
        logger.info(f"\n=== Verifying round-trip for variant: {variant} ===")

        # Extract original values from source files (using files_parsed from config)
        original_data = self.extract_original_fortran_values(variant, config)
        if not original_data:
            logger.warning(f"Could not extract original data for {variant}")
            return False

        original_values = original_data.get('original', {})

        # Get generated values from config
        config_categories = config.get('categories', {})
        all_success = True

        for category_name, arrays in config_categories.items():
            for array_name, config_values in arrays.items():
                # Find corresponding original values
                if array_name not in original_values:
                    # Some arrays might not have original DATA (e.g., computed values)
                    logger.debug(f"Array {array_name} not found in original source")
                    continue

                original = original_values[array_name]
                generated = config_values

                # Compare
                result = self._compare_values(
                    variant, category_name, array_name,
                    original, generated
                )

                self.verification_results[variant].append(result)

                if not result.success:
                    all_success = False
                    logger.warning(
                        f"  {array_name}: MISMATCH ({len(result.original_values)} vs "
                        f"{len(result.generated_values)} values)"
                    )
                else:
                    logger.info(f"  {array_name}: OK ({len(result.generated_values)} values)")

        return all_success

    def _compare_values(self, variant: str, category: str, array_name: str,
                       original: List[float], generated: List[float],
                       tolerance: float = 1e-6) -> VerificationResult:
        """Compare original and generated values."""
        discrepancies = []

        # Check length match
        if len(original) != len(generated):
            discrepancies.append(
                f"Length mismatch: original={len(original)}, generated={len(generated)}"
            )

        # Check values
        min_len = min(len(original), len(generated))
        for i in range(min_len):
            orig_val = float(original[i])
            gen_val = float(generated[i])

            # For integer-like values, compare exactly
            if orig_val == int(orig_val) and gen_val == int(gen_val):
                if int(orig_val) != int(gen_val):
                    discrepancies.append(
                        f"Index {i}: {orig_val} != {gen_val}"
                    )
            else:
                # For floats, use tolerance
                if abs(orig_val - gen_val) > tolerance:
                    discrepancies.append(
                        f"Index {i}: {orig_val} != {gen_val} (diff: {abs(orig_val - gen_val)})"
                    )

        success = len(discrepancies) == 0
        return VerificationResult(
            variant=variant,
            category=category,
            array_name=array_name,
            success=success,
            original_values=original,
            generated_values=generated,
            discrepancies=discrepancies,
            value_count=len(generated)
        )

    def generate_verification_report(self, output_file: Path) -> None:
        """Generate a summary verification report."""
        with open(output_file, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("FORTRAN DATA STATEMENT ROUND-TRIP VERIFICATION REPORT\n")
            f.write("=" * 80 + "\n\n")

            # Summary by variant
            for variant in sorted(self.verification_results.keys()):
                results = self.verification_results[variant]

                passed = sum(1 for r in results if r.success)
                total = len(results)

                f.write(f"\nVariant: {variant}\n")
                f.write("-" * 40 + "\n")
                f.write(f"Total arrays checked: {total}\n")
                f.write(f"Passed: {passed}/{total}\n")

                if passed < total:
                    f.write("\nFailed arrays:\n")
                    for result in results:
                        if not result.success:
                            f.write(f"\n  {result.array_name} ({result.category})\n")
                            f.write(f"    Expected {len(result.original_values)} values, "
                                   f"got {len(result.generated_values)}\n")
                            for disc in result.discrepancies[:5]:  # Show first 5
                                f.write(f"    - {disc}\n")
                            if len(result.discrepancies) > 5:
                                f.write(f"    ... and {len(result.discrepancies) - 5} more\n")

        logger.info(f"Verification report written to: {output_file}")


def main():
    """Main entry point."""
    # Paths
    project_root = Path(__file__).parent.parent
    config_dir = project_root / "config"
    src_converted_dir = project_root / "src-converted"
    fortran_output_dir = config_dir / "fortran"

    logger.info(f"Project root: {project_root}")
    logger.info(f"Config dir: {config_dir}")
    logger.info(f"Fortran output dir: {fortran_output_dir}")

    # Initialize generator
    generator = FortranDataGenerator(
        config_dir=str(config_dir),
        output_dir=str(fortran_output_dir),
        src_dir=str(src_converted_dir)
    )

    # Variants to process (small, medium, large)
    test_variants = ['ne', 'ie', 'sn']

    logger.info(f"\nProcessing variants: {test_variants}\n")

    for variant in test_variants:
        logger.info(f"\n{'='*60}")
        logger.info(f"Processing variant: {variant}")
        logger.info(f"{'='*60}")

        # Load config
        config = generator.load_config(variant)
        if not config:
            logger.error(f"Failed to load config for {variant}")
            continue

        logger.info(f"Loaded config: maxsp={config.get('maxsp')}, "
                   f"categories={len(config.get('categories', {}))}")

        # Generate Fortran file
        fortran_content = generator.generate_fortran_file(variant, config)
        output_path = generator.write_fortran_file(variant, fortran_content)

        # Verify round-trip
        generator.verify_round_trip(variant, config)

    # Generate verification report
    report_path = fortran_output_dir / "VERIFICATION_REPORT.txt"
    generator.generate_verification_report(report_path)

    # Print summary
    logger.info(f"\n{'='*60}")
    logger.info("SUMMARY")
    logger.info(f"{'='*60}")

    for variant in test_variants:
        if variant in generator.verification_results:
            results = generator.verification_results[variant]
            passed = sum(1 for r in results if r.success)
            total = len(results)
            status = "PASS" if passed == total else "FAIL"
            logger.info(f"{variant.upper()}: {status} ({passed}/{total} arrays)")

    logger.info(f"\nGenerated files in: {fortran_output_dir}")
    logger.info(f"Verification report: {report_path}")


if __name__ == '__main__':
    main()
