#!/usr/bin/env python3
"""
fix_f77_continuations.py - Convert F77 column-6 continuations to F90 trailing &.

Handles three patterns found in the fvs-modern codebase:

1. PURE F77: Column-6 continuation marker, no trailing & on prev line.
   Before:  RESULT = A + B +
            &         C + D
   After:   RESULT = A + B + &
                      C + D

2. HALF-CONVERTED (& marker): Previous converter added trailing & but left
   column-6 & marker on continuation line.
   Before:  RESULT = A + B + &
            &         C + D
   After:   RESULT = A + B + &
                      C + D

3. HALF-CONVERTED (* or other marker): Previous converter added trailing &
   but left column-6 * or other marker. This creates double-operator bugs.
   Before:  CW = X*(CL**0.13)* &
                  *(EXP(EL)**(-0.008))
   After:   CW = X*(CL**0.13)* &
                  (EXP(EL)**(-0.008))

Also converts INCLUDE 'xxx.F77' references to INCLUDE 'xxx.f90'.

Usage:
    python3 fix_f77_continuations.py --find-all [SRC_ROOT]
    python3 fix_f77_continuations.py --dry-run [SRC_ROOT]
    python3 fix_f77_continuations.py file1.f90 file2.f90 ...
"""

import re
import sys
import os
import subprocess


def strip_inline_comment(code_line):
    """Remove trailing ! comment from a Fortran line, respecting strings."""
    in_quote = False
    quote_char = None
    for i, ch in enumerate(code_line):
        if in_quote:
            if ch == quote_char:
                in_quote = False
        else:
            if ch in ("'", '"'):
                in_quote = True
                quote_char = ch
            elif ch == '!':
                return code_line[:i].rstrip(), code_line[i:]
    return code_line.rstrip(), ''


def is_f77_continuation(line):
    """Check if a line has an F77 column-6 continuation marker.

    In fixed-form Fortran, column 6 holds a continuation character (any
    non-zero, non-space character). However, DATA statement continuation
    lines may have numeric values that happen to start at column 6.  To
    avoid false positives we require that when the marker is a digit, the
    rest of the line must not look like a data value list (digit followed
    by comma or period-digit).
    """
    if len(line) < 6:
        return False
    if line[:5].strip() != '':
        return False
    col6 = line[5]
    if col6 in ('&', '+', '$'):
        return True
    if col6 in ('.', '*'):
        # '.' or '*' at column 6 is continuation only if the rest of the
        # line is not a data value (e.g. ".278," would be a value)
        rest = line[6:].lstrip()
        if rest and rest[0].isdigit():
            return False
        return True
    if col6 in ('1', '2', '3', '4', '5', '6', '7', '8', '9'):
        # Digits at column 6 are ambiguous: could be a continuation marker
        # OR the start of a numeric value/FORMAT descriptor.  We reject
        # (i.e., NOT continuation) in these cases:
        #   - Followed immediately by digit, comma, period, asterisk
        #     (DATA value like 4*32.794 or list like 9, 8, 7)
        #   - Followed immediately by a letter (FORMAT descriptor like
        #     6X, 4(, 2X, 5H — or any alphanumeric expression)
        #   - After whitespace: starts with digit, comma, period, asterisk
        rest = line[6:]
        if not rest.strip():
            return True  # bare digit on otherwise blank line => continuation
        next_ch = rest[0] if rest else ''
        # Direct adjacency: digit followed by *, digit, comma, period, or letter
        if next_ch in ('*', ',', '.') or next_ch.isdigit() or next_ch.isalpha():
            return False
        # Check for parenthesis right after digit (e.g., 4('---'))
        if next_ch == '(':
            return False
        # After stripping whitespace: if rest starts with digit/comma/period
        rest_stripped = rest.lstrip()
        if rest_stripped and (rest_stripped[0] in (',', '.', '*') or rest_stripped[0].isdigit()):
            return False
        return True
    return False


def fix_file(filepath, dry_run=False):
    """Fix all F77 continuation patterns in a file. Returns (changed, n_fixes)."""
    with open(filepath, 'r', errors='replace') as f:
        lines = f.readlines()

    new_lines = list(lines)  # work on a copy
    n_fixes = 0

    # --- Pass 1: Fix PURE F77 continuations ---
    # Where the previous line does NOT end with &
    i = 0
    result = []
    while i < len(new_lines):
        line = new_lines[i]

        if i + 1 < len(new_lines) and is_f77_continuation(new_lines[i + 1]):
            curr = line.rstrip('\n').rstrip('\r')
            code_part, comment_part = strip_inline_comment(curr)

            # Only add & if the code doesn't already end with &
            if not code_part.rstrip().endswith('&'):
                if comment_part:
                    new_line = code_part + ' & ' + comment_part + '\n'
                else:
                    new_line = code_part + ' &\n'
                result.append(new_line)
                n_fixes += 1
            else:
                result.append(line)

            # Process continuation line: remove column-6 marker but
            # preserve operator characters (+, -, .) that are both the
            # continuation marker AND part of the expression.
            i += 1
            cont = new_lines[i]
            if len(cont) > 6:
                marker = cont[5]
                if marker in ('+', '-', '.', '*'):
                    # Keep the operator character as part of the code.
                    # Use 6 spaces so the operator lands at column 7,
                    # preventing re-detection as a column-6 marker.
                    result.append('      ' + cont[5:])
                else:
                    result.append('      ' + cont[6:])
            else:
                result.append(cont)
            n_fixes += 1
        else:
            result.append(line)
        i += 1

    new_lines = result

    # --- Pass 2: Fix HALF-CONVERTED continuations ---
    # Where prev line ends with & AND current line has column-6 marker
    changed_pass2 = True
    while changed_pass2:
        changed_pass2 = False
        for i in range(1, len(new_lines)):
            prev = new_lines[i - 1].rstrip('\n').rstrip('\r')
            curr = new_lines[i]

            # Check if prev line ends with &
            prev_code, _ = strip_inline_comment(prev)
            if not prev_code.rstrip().endswith('&'):
                continue

            # Check if current line has column-6 marker
            if not is_f77_continuation(curr):
                continue

            # Remove the column-6 marker but preserve operator characters
            if len(curr) > 6:
                marker = curr[5]
                if marker in ('+', '-', '.', '*'):
                    new_lines[i] = '      ' + curr[5:]
                else:
                    new_lines[i] = '      ' + curr[6:]
            n_fixes += 1
            changed_pass2 = True

    # --- Pass 3: Fix remaining double-operator patterns ---
    # Lines like "* &\n          *(expr)" where the * at start of continuation
    # was a column-6 marker but is now at arbitrary indentation
    for i in range(1, len(new_lines)):
        prev = new_lines[i - 1].rstrip('\n').rstrip('\r')
        curr = new_lines[i]

        prev_code, _ = strip_inline_comment(prev)
        if not prev_code.rstrip().endswith('&'):
            continue

        # Get the code before the trailing & (strip & and whitespace)
        prev_stripped = prev_code.rstrip()
        code_before_amp = prev_stripped[:-1].rstrip()  # remove & and trailing spaces
        if not code_before_amp:
            continue

        # Get the last non-space character before the &
        last_char = code_before_amp[-1]

        # Check if current line starts with an operator
        curr_stripped = curr.lstrip()
        if not curr_stripped:
            continue
        first_char = curr_stripped[0]

        # Fix double-operator: last char is an operator AND first char of
        # continuation is the same operator (e.g., "* &" followed by "*(expr)")
        if last_char in ('*', '+', '-') and first_char == last_char:
            # Remove the leading operator from the continuation line
            indent = len(curr) - len(curr.lstrip())
            new_lines[i] = curr[:indent] + curr_stripped[1:]
            n_fixes += 1

    # --- Pass 4: Fix .F77 include references ---
    for j, line in enumerate(new_lines):
        if re.search(r"INCLUDE\s+'[^']*\.F77'", line, re.IGNORECASE):
            new_line = re.sub(
                r"(INCLUDE\s+'[^']*\.)F77(')",
                r'\1f90\2', line, flags=re.IGNORECASE
            )
            if new_line != line:
                new_lines[j] = new_line
                n_fixes += 1

    if n_fixes == 0:
        return False, 0

    if not dry_run:
        with open(filepath, 'w') as f:
            f.writelines(new_lines)

    return True, n_fixes


def find_files_needing_fix(src_root):
    """Find .f90 files with F77 continuation patterns."""
    # Pattern 1: column-6 continuation markers (& + * . $ digits)
    result = subprocess.run(
        ['grep', '-rlP', r'^ {5}[&+.*$1-9]', src_root, '--include=*.f90'],
        capture_output=True, text=True
    )
    files = set(f for f in result.stdout.strip().split('\n') if f)

    # Pattern 2: .F77 include references
    result2 = subprocess.run(
        ['grep', '-rli', r"\.F77'", src_root, '--include=*.f90'],
        capture_output=True, text=True
    )
    files.update(f for f in result2.stdout.strip().split('\n') if f)

    # Pattern 3: double-operator from half-converted continuations
    # Find files where a line ends with "* &" (multiplication then continuation)
    result3 = subprocess.run(
        ['grep', '-rlP', r'\*\s*&\s*$', src_root, '--include=*.f90'],
        capture_output=True, text=True
    )
    # Only include files where the next line also starts with * (double-operator)
    for fpath in result3.stdout.strip().split('\n'):
        if not fpath or not os.path.isfile(fpath):
            continue
        try:
            with open(fpath, 'r', errors='replace') as fh:
                flines = fh.readlines()
            for j in range(len(flines) - 1):
                if flines[j].rstrip().endswith('&'):
                    code, _ = strip_inline_comment(flines[j].rstrip())
                    code_no_amp = code.rstrip()[:-1].rstrip() if code.rstrip().endswith('&') else ''
                    if code_no_amp and code_no_amp[-1] == '*':
                        nxt = flines[j + 1].lstrip()
                        if nxt.startswith('*'):
                            files.add(fpath)
                            break
        except Exception:
            pass

    # Exclude archive and metric directories
    files = sorted(f for f in files if '/archive/' not in f and '/metric/' not in f)
    return files


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    dry_run = False
    if sys.argv[1] == '--dry-run':
        dry_run = True
        src_root = sys.argv[2] if len(sys.argv) > 2 else 'src-converted'
        files = find_files_needing_fix(src_root)
        print(f"Found {len(files)} files needing fixes (dry run)")
    elif sys.argv[1] == '--find-all':
        src_root = sys.argv[2] if len(sys.argv) > 2 else 'src-converted'
        files = find_files_needing_fix(src_root)
        print(f"Found {len(files)} files needing fixes")
    else:
        files = sys.argv[1:]

    total_fixed = 0
    total_files = 0
    for filepath in files:
        if not os.path.isfile(filepath):
            print(f"  SKIP: {filepath} (not found)")
            continue
        changed, n = fix_file(filepath, dry_run=dry_run)
        if changed:
            action = "WOULD FIX" if dry_run else "FIXED"
            print(f"  {action}: {filepath} ({n} fixes)")
            total_fixed += n
            total_files += 1

    print(f"\nSummary: {'Would fix' if dry_run else 'Fixed'} "
          f"{total_fixed} issues in {total_files} files")


if __name__ == '__main__':
    main()
