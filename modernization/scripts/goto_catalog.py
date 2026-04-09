#!/usr/bin/env python3
"""
GOTO Catalog Script for FVS Fortran Modernization
Scans all .f files in the FVS codebase and identifies/categorizes GOTO patterns.
"""

import os
import re
from collections import defaultdict
from pathlib import Path


class GOTOCatalog:
    def __init__(self, root_dir):
        self.root_dir = root_dir
        self.gotos = []
        self.stats = {
            'simple': 0,
            'computed': 0,
            'assigned': 0,
            'arithmetic_if': 0
        }
        self.directory_stats = defaultdict(lambda: defaultdict(int))
        self.file_stats = {}

    def find_fortran_files(self):
        """Find all Fortran source files."""
        fortran_files = []
        for root, dirs, files in os.walk(self.root_dir):
            for file in files:
                if file.lower().endswith(('.f', '.f77', '.for')):
                    fortran_files.append(os.path.join(root, file))
        return sorted(fortran_files)

    def extract_context(self, lines, line_num, context_lines=2):
        """Extract surrounding context around a match."""
        start = max(0, line_num - context_lines)
        end = min(len(lines), line_num + context_lines + 1)
        return '\n'.join(f"{i+1:5d}: {lines[i]}" for i in range(start, end))

    def find_simple_goto(self, line, line_num, lines, file_path):
        """Match: GO TO nnn or GOTO nnn"""
        # Case-insensitive match for GO TO or GOTO followed by a number
        match = re.search(r'\b(?:GO\s+TO|GOTO)\s+(\d+)\b', line, re.IGNORECASE)
        if match:
            label = match.group(1)
            self.gotos.append({
                'file': file_path,
                'line': line_num,
                'type': 'Simple GOTO',
                'pattern': f"GO TO {label}",
                'targets': [label],
                'context': self.extract_context(lines, line_num - 1),
                'code': line.strip()
            })
            self.stats['simple'] += 1
            return True
        return False

    def find_computed_goto(self, line, line_num, lines, file_path):
        """Match: GO TO (n1, n2, ...), expr"""
        match = re.search(r'\b(?:GO\s+TO|GOTO)\s*\(\s*(\d+(?:\s*,\s*\d+)*)\s*\)\s*,\s*(.+)',
                          line, re.IGNORECASE)
        if match:
            labels = re.findall(r'\d+', match.group(1))
            expr = match.group(2).strip()
            self.gotos.append({
                'file': file_path,
                'line': line_num,
                'type': 'Computed GOTO',
                'pattern': f"GO TO ({', '.join(labels)}), {expr}",
                'targets': labels,
                'expression': expr,
                'context': self.extract_context(lines, line_num - 1),
                'code': line.strip()
            })
            self.stats['computed'] += 1
            return True
        return False

    def find_assigned_goto(self, lines, line_num, file_path):
        """Match: ASSIGN nnn TO var / GO TO var"""
        # Look for ASSIGN pattern
        if line_num > 0:
            prev_line = lines[line_num - 1]
            assign_match = re.search(r'\bASSIGN\s+(\d+)\s+TO\s+(\w+)', prev_line, re.IGNORECASE)

            curr_line = lines[line_num]
            goto_match = re.search(r'\b(?:GO\s+TO|GOTO)\s+(\w+)\b', curr_line, re.IGNORECASE)

            if assign_match and goto_match:
                label = assign_match.group(1)
                var = assign_match.group(2)
                goto_var = goto_match.group(1)

                if var.lower() == goto_var.lower():
                    self.gotos.append({
                        'file': file_path,
                        'line': line_num,
                        'type': 'Assigned GOTO',
                        'pattern': f"ASSIGN {label} TO {var} / GO TO {var}",
                        'targets': [label],
                        'variable': var,
                        'context': self.extract_context(lines, line_num - 2),
                        'code': f"{prev_line.strip()} / {curr_line.strip()}"
                    })
                    self.stats['assigned'] += 1
                    return True
        return False

    def find_arithmetic_if(self, line, line_num, lines, file_path):
        """Match: IF (expr) n1, n2, n3"""
        match = re.search(r'\bIF\s*\(\s*(.+?)\s*\)\s+(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\b',
                          line, re.IGNORECASE)
        if match:
            expr = match.group(1)
            labels = [match.group(2), match.group(3), match.group(4)]
            self.gotos.append({
                'file': file_path,
                'line': line_num,
                'type': 'Arithmetic IF',
                'pattern': f"IF ({expr}) {', '.join(labels)}",
                'targets': labels,
                'expression': expr,
                'context': self.extract_context(lines, line_num - 1),
                'code': line.strip()
            })
            self.stats['arithmetic_if'] += 1
            return True
        return False

    def scan_file(self, file_path):
        """Scan a single Fortran file for GOTO patterns."""
        try:
            with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
                lines = f.readlines()

            dir_name = os.path.dirname(file_path)
            rel_dir = os.path.relpath(dir_name, self.root_dir)
            line_count = len(lines)
            self.file_stats[file_path] = {'lines': line_count, 'gotos': 0}

            for i, line in enumerate(lines):
                # Skip comments
                if line.strip().startswith('C') or line.strip().startswith('!'):
                    continue

                # Check for GOTO patterns
                found = False
                found |= self.find_simple_goto(line, i + 1, lines, file_path)
                found |= self.find_computed_goto(line, i + 1, lines, file_path)
                found |= self.find_arithmetic_if(line, i + 1, lines, file_path)

                # Check for assigned GOTO (two-line pattern)
                if i < len(lines) - 1:
                    found |= self.find_assigned_goto(lines, i + 1, file_path)

                if found:
                    self.file_stats[file_path]['gotos'] += 1
                    self.directory_stats[rel_dir][self.gotos[-1]['type']] += 1

        except Exception as e:
            print(f"Error reading {file_path}: {e}")

    def scan_all(self):
        """Scan all Fortran files."""
        files = self.find_fortran_files()
        print(f"Found {len(files)} Fortran files")
        for i, file_path in enumerate(files):
            if (i + 1) % 100 == 0:
                print(f"  Processing file {i+1}/{len(files)}...")
            self.scan_file(file_path)

    def calculate_density(self):
        """Calculate GOTO density (GOTOs per 100 lines) for each file."""
        density_list = []
        for file_path, stats in self.file_stats.items():
            if stats['gotos'] > 0:
                density = (stats['gotos'] / stats['lines'] * 100) if stats['lines'] > 0 else 0
                density_list.append({
                    'file': file_path,
                    'gotos': stats['gotos'],
                    'lines': stats['lines'],
                    'density': density
                })

        density_list.sort(key=lambda x: x['density'], reverse=True)
        return density_list

    def generate_report(self):
        """Generate a comprehensive report."""
        report = []
        report.append("# FVS Fortran GOTO Catalog Report\n")
        report.append(f"Generated from: {self.root_dir}\n")

        # Summary statistics
        report.append("\n## Summary Statistics\n")
        total_gotos = sum(self.stats.values())
        report.append(f"**Total GOTO Patterns Found: {total_gotos}**\n\n")

        report.append("### GOTO Type Distribution\n")
        report.append("| Type | Count | Percentage |\n")
        report.append("|------|-------|------------|\n")
        for goto_type, count in self.stats.items():
            pct = (count / total_gotos * 100) if total_gotos > 0 else 0
            type_label = goto_type.replace('_', ' ').title()
            report.append(f"| {type_label} | {count} | {pct:.1f}% |\n")

        # Directory statistics
        report.append("\n## Statistics by Directory\n")
        report.append("| Directory | Simple | Computed | Assigned | Arithmetic IF | Total |\n")
        report.append("|-----------|--------|----------|----------|---------------|-----------|\n")

        for dir_name in sorted(self.directory_stats.keys()):
            stats = self.directory_stats[dir_name]
            total = sum(stats.values())
            if total > 0:
                report.append(f"| {dir_name} | {stats.get('Simple GOTO', 0)} | "
                            f"{stats.get('Computed GOTO', 0)} | {stats.get('Assigned GOTO', 0)} | "
                            f"{stats.get('Arithmetic IF', 0)} | {total} |\n")

        # Files ranked by GOTO density
        report.append("\n## Top 20 Files by GOTO Density (GOTOs per 100 lines)\n")
        density_list = self.calculate_density()
        report.append("| Rank | File | GOTOs | Lines | Density |\n")
        report.append("|------|------|-------|-------|----------|\n")

        for rank, item in enumerate(density_list[:20], 1):
            rel_path = os.path.relpath(item['file'], self.root_dir)
            report.append(f"| {rank} | {rel_path} | {item['gotos']} | "
                        f"{item['lines']} | {item['density']:.2f}% |\n")

        # Modern Fortran replacements
        report.append("\n## Modern Fortran Replacement Patterns\n")

        report.append("\n### 1. Simple GOTO (GO TO nnn)\n")
        report.append("**Before (Legacy Fortran):**\n")
        report.append("```fortran\n")
        report.append("      IF (error) GO TO 999\n")
        report.append("      ! ... do work ...\n")
        report.append(" 999  CONTINUE\n")
        report.append("      STOP\n")
        report.append("```\n\n")
        report.append("**After (Modern Fortran):**\n")
        report.append("```fortran\n")
        report.append("      IF (error) THEN\n")
        report.append("          ! error handling\n")
        report.append("          STOP\n")
        report.append("      END IF\n")
        report.append("      ! ... do work ...\n")
        report.append("```\n\n")

        report.append("### 2. Computed GOTO (GO TO (n1, n2, ...), expr)\n")
        report.append("**Before (Legacy Fortran):**\n")
        report.append("```fortran\n")
        report.append("      GO TO (10, 20, 30, 40), icase\n")
        report.append(" 10   CONTINUE\n")
        report.append("      ! ... handle case 1 ...\n")
        report.append("      GO TO 100\n")
        report.append(" 20   CONTINUE\n")
        report.append("      ! ... handle case 2 ...\n")
        report.append("      GO TO 100\n")
        report.append(" 30   CONTINUE\n")
        report.append("      ! ... handle case 3 ...\n")
        report.append(" 100  CONTINUE\n")
        report.append("```\n\n")
        report.append("**After (Modern Fortran):**\n")
        report.append("```fortran\n")
        report.append("      SELECT CASE (icase)\n")
        report.append("          CASE (1)\n")
        report.append("              ! ... handle case 1 ...\n")
        report.append("          CASE (2)\n")
        report.append("              ! ... handle case 2 ...\n")
        report.append("          CASE (3)\n")
        report.append("              ! ... handle case 3 ...\n")
        report.append("          CASE DEFAULT\n")
        report.append("              ! handle unexpected case\n")
        report.append("      END SELECT\n")
        report.append("```\n\n")

        report.append("### 3. Assigned GOTO (ASSIGN n TO var / GO TO var)\n")
        report.append("**Before (Legacy Fortran):**\n")
        report.append("```fortran\n")
        report.append("      IF (condition1) THEN\n")
        report.append("          ASSIGN 100 TO next_label\n")
        report.append("      ELSE IF (condition2) THEN\n")
        report.append("          ASSIGN 200 TO next_label\n")
        report.append("      END IF\n")
        report.append("      ! ... do work ...\n")
        report.append("      GO TO next_label\n")
        report.append(" 100  CONTINUE\n")
        report.append("      ! ... path 1 ...\n")
        report.append(" 200  CONTINUE\n")
        report.append("      ! ... path 2 ...\n")
        report.append("```\n\n")
        report.append("**After (Modern Fortran):**\n")
        report.append("```fortran\n")
        report.append("      INTEGER :: next_path\n")
        report.append("      IF (condition1) THEN\n")
        report.append("          next_path = 1\n")
        report.append("      ELSE IF (condition2) THEN\n")
        report.append("          next_path = 2\n")
        report.append("      END IF\n")
        report.append("      ! ... do work ...\n")
        report.append("      SELECT CASE (next_path)\n")
        report.append("          CASE (1)\n")
        report.append("              ! ... path 1 ...\n")
        report.append("          CASE (2)\n")
        report.append("              ! ... path 2 ...\n")
        report.append("      END SELECT\n")
        report.append("```\n\n")

        report.append("### 4. Arithmetic IF (IF (expr) n1, n2, n3)\n")
        report.append("**Before (Legacy Fortran):**\n")
        report.append("```fortran\n")
        report.append("      IF (value - target) 10, 20, 30\n")
        report.append(" 10   CONTINUE\n")
        report.append("      ! ... value is negative (less than target) ...\n")
        report.append("      GO TO 100\n")
        report.append(" 20   CONTINUE\n")
        report.append("      ! ... value is zero (equal to target) ...\n")
        report.append("      GO TO 100\n")
        report.append(" 30   CONTINUE\n")
        report.append("      ! ... value is positive (greater than target) ...\n")
        report.append(" 100  CONTINUE\n")
        report.append("```\n\n")
        report.append("**After (Modern Fortran):**\n")
        report.append("```fortran\n")
        report.append("      IF (value < target) THEN\n")
        report.append("          ! ... value is negative (less than target) ...\n")
        report.append("      ELSE IF (value == target) THEN\n")
        report.append("          ! ... value is zero (equal to target) ...\n")
        report.append("      ELSE IF (value > target) THEN\n")
        report.append("          ! ... value is positive (greater than target) ...\n")
        report.append("      END IF\n")
        report.append("```\n\n")

        # Sample detections
        report.append("\n## Sample GOTO Detections\n\n")

        # Group by type
        by_type = defaultdict(list)
        for goto in self.gotos:
            by_type[goto['type']].append(goto)

        for goto_type in sorted(by_type.keys()):
            report.append(f"### {goto_type}s (showing first 5)\n")
            for goto in by_type[goto_type][:5]:
                rel_path = os.path.relpath(goto['file'], self.root_dir)
                report.append(f"**File:** {rel_path}:{goto['line']}\n")
                report.append(f"**Code:** {goto['code']}\n")
                report.append(f"**Targets:** {', '.join(goto['targets'])}\n")
                report.append("```\n" + goto['context'] + "\n```\n\n")

        return ''.join(report)


def main():
    root_dir = 'os.environ.get("HOME", "/home") + "/path/to/ForestVegetationSimulator-main'
    catalog = GOTOCatalog(root_dir)

    print("Scanning FVS codebase for GOTO patterns...")
    catalog.scan_all()

    print(f"\nFound {len(catalog.gotos)} GOTO patterns:")
    print(f"  Simple GOTO: {catalog.stats['simple']}")
    print(f"  Computed GOTO: {catalog.stats['computed']}")
    print(f"  Assigned GOTO: {catalog.stats['assigned']}")
    print(f"  Arithmetic IF: {catalog.stats['arithmetic_if']}")

    print("\nGenerating report...")
    report = catalog.generate_report()

    output_path = 'modernization/reports/goto_catalog_report.md'
    with open(output_path, 'w') as f:
        f.write(report)

    print(f"Report written to: {output_path}")


if __name__ == '__main__':
    main()
