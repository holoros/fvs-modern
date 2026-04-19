#!/usr/bin/env python3
"""
audit_nvel_upstream.py

Compare fvs-modern's .inc coefficient files against the upstream
FMSC VolumeLibrary to detect numeric divergence.

The comparison is *numeric*, not textual. Fortran 77 vs. F90 formatting
differences (comment syntax, continuation style, whitespace) are ignored.
Only actual numeric values embedded in DATA statements are compared.

Usage:
    python3 scripts/audit_nvel_upstream.py [--upstream DIR] [--local DIR] [--verbose]

Defaults:
    --upstream  upstream/VolumeLibrary   (git submodule)
    --local     src-converted/volume/NVEL

Exit codes:
    0  All .inc files match numerically
    1  One or more files have numeric differences
    2  Script error (missing directories, etc.)
"""

import argparse
import os
import re
import sys
from pathlib import Path


def extract_numbers(filepath: Path) -> list[float]:
    """
    Extract all numeric literals from a Fortran include file,
    ignoring comments, continuation markers, and string literals.

    Returns a flat list of floats in file order.
    """
    numbers = []
    text = filepath.read_text(encoding="utf-8", errors="replace")

    for line in text.splitlines():
        stripped = line.strip()

        # Skip empty lines
        if not stripped:
            continue

        # Skip F90-style comment lines (! in column 1)
        if stripped[0] == "!":
            continue
        # Fixed-form: C or c in column 1 (check original line, not stripped)
        raw = line.rstrip()
        if raw and raw[0] in ("C", "c") and not raw[0:4].upper().startswith("CHAR"):
            continue
        # Fixed-form: * in column 1 is a comment (NOT column 6 continuation)
        if raw and raw[0] == "*":
            continue

        # Fixed-form continuation: column 6 is non-blank and non-zero
        # Strip the continuation marker so it doesn't get parsed as data
        if len(raw) >= 6 and raw[0:5] == "     " and raw[5] not in (" ", "0", ""):
            raw = "      " + raw[6:] if len(raw) > 6 else ""
            stripped = raw.strip()
            if not stripped:
                continue

        # Remove inline comments (! not inside quotes)
        in_quote = False
        quote_char = None
        clean = []
        for ch in stripped:
            if in_quote:
                if ch == quote_char:
                    in_quote = False
                clean.append(ch)
            else:
                if ch == "!":
                    break
                if ch in ("'", '"'):
                    in_quote = True
                    quote_char = ch
                clean.append(ch)
        stripped = "".join(clean)

        # Remove string literals (single- and double-quoted)
        stripped = re.sub(r"'[^']*'", " ", stripped)
        stripped = re.sub(r'"[^"]*"', " ", stripped)

        # Remove Fortran keywords that look numeric-adjacent
        stripped = re.sub(
            r"\b(DATA|INTEGER|REAL|CHARACTER|DIMENSION|PARAMETER|IMPLICIT|INCLUDE"
            r"|NONE|TOTDFT)\b",
            " ",
            stripped,
            flags=re.IGNORECASE,
        )

        # Remove array indexing like (I1, J1) and loop constructs
        stripped = re.sub(r"\([A-Za-z][A-Za-z0-9]*\s*,\s*[A-Za-z][A-Za-z0-9]*\)", " ", stripped)
        stripped = re.sub(r"\b[A-Za-z_][A-Za-z0-9_]*\s*=\s*\d+\s*,\s*\d+", " ", stripped)

        # Remove variable names (identifiers not starting with a digit)
        stripped = re.sub(r"\b[A-Za-z_][A-Za-z0-9_]*\b", " ", stripped)

        # Remove continuation characters and separators
        stripped = stripped.replace("&", " ").replace("/", " ").replace(",", " ")
        stripped = stripped.replace("(", " ").replace(")", " ")
        stripped = stripped.replace("*", " ")

        # Extract numeric tokens
        for token in stripped.split():
            token = token.strip()
            if not token:
                continue
            try:
                numbers.append(float(token))
            except ValueError:
                pass

    return numbers


def find_upstream_match(local_name: str, upstream_dir: Path) -> Path | None:
    """
    Find the upstream file matching a local .inc filename.
    Case-insensitive search across the upstream tree.
    """
    target = local_name.lower()
    for p in upstream_dir.rglob("*"):
        if p.is_file() and p.name.lower() == target:
            return p
    return None


def compare_files(local_path: Path, upstream_path: Path, verbose: bool = False) -> dict:
    """
    Compare two .inc files numerically.

    Returns a dict with:
        match: bool
        local_count: int
        upstream_count: int
        differences: int (count of values that differ)
        extra_upstream: int (additional values in upstream)
        extra_local: int (additional values in local)
    """
    local_nums = extract_numbers(local_path)
    upstream_nums = extract_numbers(upstream_path)

    min_len = min(len(local_nums), len(upstream_nums))
    diffs = 0
    diff_positions = []

    for i in range(min_len):
        if abs(local_nums[i] - upstream_nums[i]) > 1e-10:
            diffs += 1
            if verbose and len(diff_positions) < 10:
                diff_positions.append((i, local_nums[i], upstream_nums[i]))

    result = {
        "match": diffs == 0 and len(local_nums) == len(upstream_nums),
        "local_count": len(local_nums),
        "upstream_count": len(upstream_nums),
        "differences": diffs,
        "extra_upstream": max(0, len(upstream_nums) - len(local_nums)),
        "extra_local": max(0, len(local_nums) - len(upstream_nums)),
    }

    if verbose and diff_positions:
        result["sample_diffs"] = diff_positions

    return result


def main():
    parser = argparse.ArgumentParser(
        description="Audit fvs-modern NVEL .inc files against upstream VolumeLibrary"
    )
    parser.add_argument(
        "--upstream",
        default="upstream/VolumeLibrary",
        help="Path to upstream VolumeLibrary (default: upstream/VolumeLibrary)",
    )
    parser.add_argument(
        "--local",
        default="src-converted/volume/NVEL",
        help="Path to local NVEL .inc files (default: src-converted/volume/NVEL)",
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Show sample differences")
    parser.add_argument(
        "--allow-differ",
        nargs="*",
        default=[],
        help="Files allowed to differ without failing (e.g. regndftdata.inc)",
    )
    args = parser.parse_args()

    # Resolve paths relative to repo root
    repo_root = Path(__file__).resolve().parent.parent
    upstream_dir = Path(args.upstream)
    local_dir = Path(args.local)

    if not upstream_dir.is_absolute():
        upstream_dir = repo_root / upstream_dir
    if not local_dir.is_absolute():
        local_dir = repo_root / local_dir

    if not upstream_dir.is_dir():
        print(f"ERROR: Upstream directory not found: {upstream_dir}", file=sys.stderr)
        print("       Have you initialized the git submodule?", file=sys.stderr)
        print("       Run: git submodule update --init upstream/VolumeLibrary", file=sys.stderr)
        sys.exit(2)

    if not local_dir.is_dir():
        print(f"ERROR: Local NVEL directory not found: {local_dir}", file=sys.stderr)
        sys.exit(2)

    # Find all local .inc files
    local_incs = sorted(local_dir.glob("*.inc"))
    if not local_incs:
        print(f"WARNING: No .inc files found in {local_dir}", file=sys.stderr)
        sys.exit(2)

    print(f"NVEL Upstream Audit")
    print(f"{'=' * 70}")
    print(f"Local:    {local_dir}")
    print(f"Upstream: {upstream_dir}")
    print(f"Files:    {len(local_incs)} .inc files")
    print(f"{'=' * 70}")
    print()

    # Build allowlist of files that may differ without failing CI
    allow_set = {name.lower() for name in args.allow_differ}

    matched = 0
    diverged = 0
    diverged_allowed = 0
    missing = 0
    results = []

    for inc in local_incs:
        upstream_match = find_upstream_match(inc.name, upstream_dir)

        if upstream_match is None:
            print(f"  [{inc.name:30s}]  NOT FOUND UPSTREAM")
            missing += 1
            continue

        result = compare_files(inc, upstream_match, verbose=args.verbose)

        if result["match"]:
            print(f"  [{inc.name:30s}]  MATCH  ({result['local_count']} values)")
            matched += 1
        else:
            allowed = inc.name.lower() in allow_set
            tag = "DIFFERS (allowed)" if allowed else "DIFFERS"
            detail = []
            if result["differences"] > 0:
                detail.append(f"{result['differences']} value diffs")
            if result["extra_upstream"] > 0:
                detail.append(f"+{result['extra_upstream']} upstream")
            if result["extra_local"] > 0:
                detail.append(f"+{result['extra_local']} local")
            detail_str = ", ".join(detail)
            print(
                f"  [{inc.name:30s}]  {tag}  "
                f"(local={result['local_count']}, upstream={result['upstream_count']}; {detail_str})"
            )
            if args.verbose and "sample_diffs" in result:
                for pos, lv, uv in result["sample_diffs"]:
                    print(f"      position {pos}: local={lv}, upstream={uv}")
            if allowed:
                diverged_allowed += 1
            else:
                diverged += 1
            results.append((inc.name, result))

    print()
    print(f"{'=' * 70}")
    summary_parts = [f"{matched} match", f"{diverged} differ"]
    if diverged_allowed > 0:
        summary_parts.append(f"{diverged_allowed} differ (allowed)")
    summary_parts.append(f"{missing} not found upstream")
    print(f"Summary: {', '.join(summary_parts)}")
    print(f"{'=' * 70}")

    if diverged > 0:
        print()
        print("Files with unexpected numeric differences should be reviewed.")
        print("See docs/nvel_integration_recommendations.md for the backport procedure.")
        print("Use --allow-differ <file> to mark intentional divergences.")
        sys.exit(1)
    else:
        print()
        if diverged_allowed > 0:
            print(f"All .inc files match or have allowed differences ({diverged_allowed} allowed).")
        else:
            print("All .inc coefficient files match upstream numerically.")
        sys.exit(0)


if __name__ == "__main__":
    main()
