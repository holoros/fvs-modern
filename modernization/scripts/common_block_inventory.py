#!/usr/bin/env python3
"""
COMMON Block Inventory Script for FVS Fortran Modernization
Scans the codebase to list INCLUDE statements and track dependencies.
"""

import os
import re
from collections import defaultdict
from pathlib import Path


class CommonBlockInventory:
    def __init__(self, root_dir):
        self.root_dir = root_dir
        self.includes = defaultdict(set)  # file -> set of included common blocks
        self.block_users = defaultdict(set)  # common block -> set of files using it
        self.block_info = {}  # common block path -> file info
        self.all_common_blocks = set()

    def find_fortran_files(self):
        """Find all Fortran source files."""
        fortran_files = []
        for root, dirs, files in os.walk(self.root_dir):
            for file in files:
                if file.lower().endswith(('.f', '.f77', '.for')):
                    fortran_files.append(os.path.join(root, file))
        return sorted(fortran_files)

    def scan_file_for_includes(self, file_path):
        """Scan a file for INCLUDE statements."""
        try:
            with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
                lines = f.readlines()

            rel_path = os.path.relpath(file_path, self.root_dir)

            for line in lines:
                # Look for INCLUDE statements (case-insensitive)
                match = re.search(r"^\s*INCLUDE\s+'([^']+)'", line, re.IGNORECASE)
                if not match:
                    match = re.search(r'^\s*INCLUDE\s+"([^"]+)"', line, re.IGNORECASE)

                if match:
                    include_file = match.group(1)
                    self.includes[rel_path].add(include_file)
                    self.block_users[include_file].add(rel_path)
                    self.all_common_blocks.add(include_file)

        except Exception as e:
            print(f"Error reading {file_path}: {e}")

    def scan_all(self):
        """Scan all Fortran files."""
        files = self.find_fortran_files()
        print(f"Found {len(files)} Fortran files")
        for i, file_path in enumerate(files):
            if (i + 1) % 100 == 0:
                print(f"  Processing file {i+1}/{len(files)}...")
            self.scan_file_for_includes(file_path)

    def get_block_file_path(self, block_name):
        """Try to find the actual path to a common block file."""
        for root, dirs, files in os.walk(self.root_dir):
            for file in files:
                if file.upper() == block_name.upper():
                    return os.path.join(root, file)
        return None

    def get_block_size(self, block_path):
        """Get the size of a common block file."""
        try:
            full_path = self.get_block_file_path(block_path)
            if full_path and os.path.exists(full_path):
                return os.path.getsize(full_path)
        except:
            pass
        return 0

    def generate_inventory_report(self):
        """Generate a comprehensive inventory report."""
        report = []
        report.append("# FVS COMMON Block Inventory\n")
        report.append(f"Generated from: {self.root_dir}\n\n")

        # Summary
        report.append("## Summary\n")
        report.append(f"* Total unique COMMON block files referenced: {len(self.all_common_blocks)}\n")
        report.append(f"* Total source files: {len(self.includes)}\n")
        report.append(f"* Files with INCLUDE statements: {len([f for f in self.includes if self.includes[f]])}\n\n")

        # List of all common blocks with usage
        report.append("## All COMMON Block Files and Their Usage\n\n")

        blocks_sorted = sorted(self.all_common_blocks)
        report.append("| COMMON Block | Number of Users | User Files |\n")
        report.append("|--------------|-----------------|---------------|\n")

        for block in blocks_sorted:
            users = self.block_users[block]
            user_count = len(users)
            user_files_list = ', '.join(sorted(list(users))[:3])
            if user_count > 3:
                user_files_list += f", ... ({user_count - 3} more)"
            report.append(f"| {block} | {user_count} | {user_files_list} |\n")

        # Top common blocks by usage
        report.append("\n## Top 20 Most Used COMMON Blocks\n\n")
        sorted_blocks = sorted(self.all_common_blocks,
                              key=lambda b: len(self.block_users[b]), reverse=True)
        report.append("| Rank | COMMON Block | Usage Count |\n")
        report.append("|------|--------------|-------------|\n")

        for rank, block in enumerate(sorted_blocks[:20], 1):
            usage = len(self.block_users[block])
            report.append(f"| {rank} | {block} | {usage} |\n")

        # Dependency matrix (limited to top blocks)
        report.append("\n## Dependency Matrix (Top 15 Blocks)\n\n")
        report.append("This matrix shows which COMMON blocks are used together in the same files.\n\n")

        top_blocks = sorted_blocks[:15]
        report.append("| Source File | " + " | ".join(top_blocks) + " |\n")
        report.append("|" + "".join(["-" * (len(b) + 2) for b in top_blocks]) + "|\n")

        # For each file, show which blocks it uses
        files_to_show = sorted([f for f in self.includes if self.includes[f]], key=lambda f: len(self.includes[f]), reverse=True)[:20]

        for file in files_to_show:
            file_blocks = self.includes[file]
            row = [file]
            for block in top_blocks:
                row.append("X" if block in file_blocks else "")
            report.append("| " + " | ".join(row) + " |\n")

        # Files grouped by COMMON blocks used
        report.append("\n## Common Block Dependencies\n\n")

        for block in sorted_blocks[:10]:
            users = sorted(self.block_users[block])
            report.append(f"\n### {block}\n")
            report.append(f"**Usage:** {len(users)} files\n\n")
            report.append("Files using this block:\n")
            for user in users[:15]:
                report.append(f"* {user}\n")
            if len(users) > 15:
                report.append(f"* ... and {len(users) - 15} more files\n")

        # Migration order suggestion
        report.append("\n## Suggested Migration Order\n\n")
        report.append("Files are ordered by number of dependencies (least to most).\n")
        report.append("Convert least-dependent blocks first to minimize ripple effects.\n\n")

        report.append("| Rank | COMMON Block | Dependencies | Recommendation |\n")
        report.append("|------|--------------|--------------|----------------|\n")

        for rank, block in enumerate(sorted_blocks, 1):
            deps = len(self.block_users[block])
            phase = "Phase 1" if deps <= 5 else "Phase 2" if deps <= 20 else "Phase 3"
            report.append(f"| {rank} | {block} | {deps} | {phase} |\n")

        return ''.join(report)

    def generate_dependency_list(self):
        """Generate a detailed dependency list."""
        output = []
        output.append("# COMMON Block Dependency Analysis\n\n")

        output.append("## File-to-Block Mapping\n\n")

        for file_path in sorted(self.includes.keys()):
            blocks = self.includes[file_path]
            if blocks:
                output.append(f"### {file_path}\n")
                output.append(f"Includes {len(blocks)} common blocks:\n")
                for block in sorted(blocks):
                    output.append(f"* {block}\n")
                output.append("\n")

        return ''.join(output)


def main():
    root_dir = '/home/aweiskittel/ForestVegetationSimulator-main'
    inventory = CommonBlockInventory(root_dir)

    print("Scanning FVS codebase for INCLUDE statements...")
    inventory.scan_all()

    print(f"\nFound:")
    print(f"  {len(inventory.all_common_blocks)} unique COMMON block files")
    print(f"  {len(inventory.includes)} source files with INCLUDE statements")

    # Generate inventory report
    print("\nGenerating inventory report...")
    report = inventory.generate_inventory_report()
    inventory_path = 'modernization/reports/common_block_inventory.md'
    with open(inventory_path, 'w') as f:
        f.write(report)
    print(f"Inventory report written to: {inventory_path}")

    # Generate dependency list
    print("\nGenerating dependency list...")
    dep_list = inventory.generate_dependency_list()
    dep_path = 'modernization/reports/common_block_dependencies.md'
    with open(dep_path, 'w') as f:
        f.write(dep_list)
    print(f"Dependency list written to: {dep_path}")


if __name__ == '__main__':
    main()
