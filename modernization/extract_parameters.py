#!/usr/bin/env python3
"""
Extract species-specific parameter DATA statements from FVS Fortran source files
and output them as structured JSON.

This script parses Fortran DATA statements including repetition syntax (e.g., "4*.9324")
and handles multi-line continuations to extract array names, values, and dimensions.
"""

import re
import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Any, Optional
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


class FortranDataExtractor:
    """Extract DATA statements from Fortran source files."""

    def __init__(self, variant_dir: str, variant_code: str):
        """Initialize extractor for a specific variant."""
        self.variant_dir = Path(variant_dir)
        self.variant_code = variant_code
        self.maxsp = None
        self.parsed_data = {}
        self.parse_errors = []
        self.files_parsed = []

    def extract_maxsp(self) -> Optional[int]:
        """Extract MAXSP parameter from PRGPRM.f90."""
        prgprm_path = self.variant_dir / "common" / "PRGPRM.f90"
        if not prgprm_path.exists():
            # Try alternative location
            prgprm_path = self.variant_dir / "PRGPRM.f90"
            if not prgprm_path.exists():
                logger.warning(f"Could not find PRGPRM.f90 for variant {self.variant_code}")
                return None

        with open(prgprm_path, 'r') as f:
            content = f.read()

        # Look for PARAMETER (MAXSP = value)
        match = re.search(r'PARAMETER\s*\(\s*MAXSP\s*=\s*(\d+)\s*\)', content, re.IGNORECASE)
        if match:
            self.maxsp = int(match.group(1))
            logger.info(f"Extracted MAXSP = {self.maxsp} for variant {self.variant_code}")
            return self.maxsp

        return None

    def read_fortran_file(self, filepath: Path) -> str:
        """Read a Fortran file and handle line continuations."""
        with open(filepath, 'r') as f:
            lines = f.readlines()

        # Join continuation lines (lines ending with & or next line starting with &)
        joined_lines = []
        i = 0
        while i < len(lines):
            line = lines[i]

            # Accumulate continued lines
            while i < len(lines) - 1 and (
                line.rstrip().endswith('&') or
                (i + 1 < len(lines) and lines[i + 1].lstrip().startswith('&'))
            ):
                # Remove trailing & and join
                line = line.rstrip()
                if line.endswith('&'):
                    line = line[:-1]
                # Get next line and remove leading &
                i += 1
                next_line = lines[i].lstrip()
                if next_line.startswith('&'):
                    next_line = next_line[1:]
                line = line + ' ' + next_line.rstrip()

            joined_lines.append(line)
            i += 1

        return '\n'.join(joined_lines)

    def parse_repeated_value(self, expr: str) -> List[float]:
        """Parse Fortran repetition syntax like '4*.9324' into list of values."""
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

    def parse_data_statement(self, data_block: str) -> Tuple[str, List[Any]]:
        """Parse a single DATA statement and extract array name and values."""
        # Normalize whitespace
        data_block = re.sub(r'\s+', ' ', data_block)

        # Extract array name and value part
        # Pattern: DATA name / values /
        match = re.match(r'DATA\s+(\w+)\s*/(.+)/\s*$', data_block, re.IGNORECASE)
        if not match:
            return None, None

        array_name = match.group(1)
        values_str = match.group(2)

        # Split by commas, handling nested parentheses
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
                    # Parse repeated values
                    parsed = self.parse_repeated_value(current_value.strip())
                    values.extend(parsed)
                current_value = ''
            else:
                current_value += char

        # Don't forget the last value
        if current_value.strip():
            parsed = self.parse_repeated_value(current_value.strip())
            values.extend(parsed)

        return array_name, values

    def extract_data_statements(self, filepath: Path) -> Dict[str, List[Any]]:
        """Extract all DATA statements from a Fortran file."""
        content = self.read_fortran_file(filepath)

        # Find all DATA statements
        # Pattern: DATA name / values /, ... more names/values pairs
        # Note: This handles both single and multiple array declarations in one DATA statement

        data_dict = {}

        # Split by 'DATA' keyword
        sections = re.split(r'\bDATA\b', content, flags=re.IGNORECASE)

        for section in sections[1:]:  # Skip first element (before first DATA)
            # Try to extract complete DATA statement(s)
            # Look for complete /.../ pairs

            # Remove comments (! and everything after on same line)
            section = re.sub(r'!.*?(?=\n|$)', '', section)

            # Find all name/values pairs: name / values /
            pattern = r'(\w+)\s*/([^/]+)/'
            matches = re.finditer(pattern, section)

            for match in matches:
                array_name = match.group(1)
                values_str = match.group(2)

                # Parse values
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
                            parsed = self.parse_repeated_value(current_value.strip())
                            values.extend(parsed)
                        current_value = ''
                    else:
                        current_value += char

                if current_value.strip():
                    parsed = self.parse_repeated_value(current_value.strip())
                    values.extend(parsed)

                if values:
                    data_dict[array_name] = values

        return data_dict

    def extract_string_data_statements(self, filepath: Path) -> Dict[str, List[str]]:
        """Extract string/character DATA statements."""
        content = self.read_fortran_file(filepath)

        string_dict = {}

        # Pattern for character/string arrays: DATA name / 'val1', 'val2', ... /
        pattern = r'DATA\s+(\w+)\s*/([^/]+)/'

        sections = re.split(r'\bDATA\b', content, flags=re.IGNORECASE)

        for section in sections[1:]:
            # Remove comments
            section = re.sub(r'!.*?(?=\n|$)', '', section)

            # Find array name and values
            match = re.match(r'\s*(\w+)\s*/([^/]+)/', section)
            if match:
                array_name = match.group(1)
                values_str = match.group(2)

                # Extract quoted strings
                strings = re.findall(r"'([^']*)'", values_str)
                if strings:
                    string_dict[array_name] = strings

        return string_dict

    def categorize_arrays(self, data_dict: Dict[str, List[Any]],
                         string_dict: Dict[str, List[str]]) -> Dict[str, Dict]:
        """Categorize arrays by their function based on naming conventions."""
        categories = {
            'bark_ratio': {},
            'growth': {},
            'crown': {},
            'height_diameter': {},
            'height_growth': {},
            'site_index': {},
            'volume': {},
            'mortality': {},
            'regeneration': {},
            'species_definitions': {},
            'other': {}
        }

        # Define patterns for categorization
        patterns = {
            'bark_ratio': ['BKRAT', 'BARK'],
            'crown': ['BCR1', 'BCR2', 'BCR3', 'BCR4', 'CRNMLT', 'DLOW', 'DHI'],
            'growth': ['B1', 'B2', 'B3', 'DGB1', 'DGB2', 'DGCOEF'],
            'height_diameter': ['SNALL', 'SNDBAL', 'HTDBH'],
            'height_growth': ['B1HT', 'B2HT', 'B3HT', 'HTCOEF'],
            'site_index': ['SDICON', 'SICOEF'],
            'species_definitions': ['JSP', 'FIAJSP', 'PLNJSP', 'NSP', 'JTYPE']
        }

        for array_name, values in data_dict.items():
            categorized = False
            for category, names in patterns.items():
                if any(pattern in array_name.upper() for pattern in names):
                    categories[category][array_name] = values
                    categorized = True
                    break

            if not categorized:
                categories['other'][array_name] = values

        # Add string arrays to species_definitions
        for array_name, values in string_dict.items():
            if any(pattern in array_name.upper() for pattern in patterns['species_definitions']):
                categories['species_definitions'][array_name] = values
            else:
                categories['other'][array_name] = values

        return categories

    def process_variant(self) -> Dict[str, Any]:
        """Process all files for a variant and extract parameters."""
        logger.info(f"\nProcessing variant: {self.variant_code}")

        # Extract MAXSP first
        self.extract_maxsp()

        if not self.maxsp:
            logger.warning(f"Could not extract MAXSP for {self.variant_code}, assuming 108")
            self.maxsp = 108

        # Files to process in order of importance
        key_files = [
            ('blkdat.f90', 'blkdat'),
            ('crown.f90', 'crown'),
            ('dgf.f90', 'diameter_growth'),
            ('htdbh.f90', 'height_diameter'),
            ('htgf.f90', 'height_growth'),
            ('sitset.f90', 'site_index'),
        ]

        all_data = {}
        all_strings = {}

        for filename, file_type in key_files:
            filepath = self.variant_dir / filename
            if filepath.exists():
                logger.info(f"  Parsing {filename}...")
                self.files_parsed.append(filename)

                try:
                    # Extract numeric data
                    data_dict = self.extract_data_statements(filepath)
                    all_data.update(data_dict)

                    # Extract string data
                    string_dict = self.extract_string_data_statements(filepath)
                    all_strings.update(string_dict)

                    logger.info(f"    Found {len(data_dict)} numeric arrays, {len(string_dict)} string arrays")
                except Exception as e:
                    error_msg = f"Error parsing {filename}: {e}"
                    logger.warning(error_msg)
                    self.parse_errors.append(error_msg)
            else:
                logger.warning(f"  File not found: {filename}")

        # Categorize the extracted data
        categorized = self.categorize_arrays(all_data, all_strings)

        # Build output structure
        output = {
            'variant': self.variant_code,
            'variant_name': self._get_variant_name(),
            'maxsp': self.maxsp,
            'files_parsed': self.files_parsed,
            'categories': {}
        }

        # Add non-empty categories
        for category, data in categorized.items():
            if data:
                output['categories'][category] = data

        return output

    def _get_variant_name(self) -> str:
        """Get human-readable variant name."""
        names = {
            'ne': 'Northeastern US',
            'sn': 'Southern',
            'ca': 'California',
            'ak': 'Alaska',
            'pn': 'Pacific Northwest',
            'wc': 'West Coast',
            'ws': 'Western Sierra',
            'cr': 'Central Rockies',
            'ut': 'Utah',
            'nc': 'North Central',
            'em': 'Eastern Mountain',
            'ci': 'Central Interior',
            'ec': 'East Central',
            'op': 'Operable',
            'oc': 'Okanogan',
            'cs': 'Central States',
            'acd': 'Alaska Coastal',
            'bm': 'Blue Mountains',
            'kt': 'Klamath',
            'ls': 'Lake States',
            'so': 'Southern Oregon',
            'tt': 'Tahoe',
            'ie': 'Inland Empire',
            'sn': 'Southern',
            'bc': 'British Columbia',
            'on': 'Ontario'
        }
        return names.get(self.variant_code.lower(), self.variant_code.upper())


def extract_all_variants(src_dir: str, output_dir: str):
    """Extract parameters for all variants."""
    src_path = Path(src_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # List of valid variant directories
    variant_dirs = [
        'acd', 'ak', 'bm', 'ca', 'ci', 'cr', 'cs', 'ec', 'em', 'ie',
        'kt', 'ls', 'nc', 'ne', 'oc', 'op', 'pn', 'sn', 'so', 'tt',
        'ut', 'wc', 'ws'
    ]

    # Check for Canada variants
    canada_path = src_path / 'canada'
    if canada_path.exists():
        for subdir in canada_path.iterdir():
            if subdir.is_dir() and subdir.name in ['bc', 'on']:
                variant_dirs.append(f'canada/{subdir.name}')

    results = {}

    for variant_code in variant_dirs:
        if variant_code.startswith('canada/'):
            variant_path = src_path / variant_code
            code = variant_code.split('/')[-1]
        else:
            variant_path = src_path / variant_code
            code = variant_code

        if not variant_path.exists():
            logger.warning(f"Variant directory not found: {variant_path}")
            continue

        # Create extractor
        extractor = FortranDataExtractor(str(variant_path), code)

        # Process variant
        try:
            output = extractor.process_variant()
            results[code] = output

            # Save to JSON
            json_path = output_path / f"{code}.json"
            with open(json_path, 'w') as f:
                json.dump(output, f, indent=2)
            logger.info(f"Saved {code}.json")

        except Exception as e:
            logger.error(f"Failed to process variant {code}: {e}")

    return results


def main():
    """Main entry point."""
    src_dir = '/home/aweiskittel/Documents/Claude/fvs-modern/src-converted'
    output_dir = '/home/aweiskittel/Documents/Claude/fvs-modern/config'

    logger.info(f"Starting FVS parameter extraction")
    logger.info(f"Source directory: {src_dir}")
    logger.info(f"Output directory: {output_dir}")

    # Extract all variants
    results = extract_all_variants(src_dir, output_dir)

    # Print summary
    logger.info(f"\n{'='*60}")
    logger.info(f"Extraction Complete")
    logger.info(f"{'='*60}")
    logger.info(f"Variants processed: {len(results)}")
    for code in sorted(results.keys()):
        info = results[code]
        logger.info(f"  {code}: {len(info['categories'])} categories, " +
                   f"{info['maxsp']} species")

    return 0


if __name__ == '__main__':
    sys.exit(main())
