#!/usr/bin/env python3
"""
Corrected Fortran 77 Fixed-Form to Fortran 90+ Free-Form Converter

Key fix: Continuation lines in Fortran 77 have a non-blank character in
column 6 of the CONTINUATION line (not the line being continued). This
converter correctly looks AHEAD to detect continuations.

Fortran 77 fixed-form layout:
  Column 1:    C, c, or * = comment line
  Columns 1-5: Statement label (numeric)
  Column 6:    Non-blank/non-zero = this line continues the PREVIOUS line
  Columns 7-72: Code

Free-form conversion:
  - Add trailing & to lines that are continued on the next line
  - Strip column 6 continuation markers
  - Convert C/c/* comments to !
  - Map INCLUDE filenames (.F77 -> .f90)
"""

import sys
import os
import re
from pathlib import Path


def is_comment_line(line):
    """Check if line is a fixed-form comment (C, c, or * in column 1)."""
    if not line:
        return False
    return line[0] in ('C', 'c', '*')


def is_blank_line(line):
    """Check if line is blank."""
    return not line.strip()


def is_continuation_line(line):
    """Check if this line has a continuation marker in column 6."""
    if len(line) < 6:
        return False
    if is_comment_line(line):
        return False
    return line[5] not in (' ', '0', '\t', '')


def get_label(line):
    """Extract statement label from columns 1-5."""
    if len(line) < 5:
        return ''
    label = line[0:5].strip()
    if label and label.isdigit():
        return label
    return ''


def get_code(line):
    """Extract code from column 7+ (index 6+)."""
    if len(line) >= 7:
        return line[6:].rstrip()
    elif len(line) > 6:
        return line[6:].rstrip()
    return ''


def fix_operator_spacing(line):
    """Fix Fortran 77 relational operator spacing for free-form.

    In fixed-form, spaces are insignificant, so 'T. EQ. 0' is valid.
    In free-form, this must be 'T .EQ. 0'.
    Also handles '.EQ .' -> '.EQ.' and '. EQ.' -> '.EQ.'
    """
    # Skip comments
    if line.lstrip().startswith('!'):
        return line

    operators = ['EQ', 'NE', 'GT', 'GE', 'LT', 'LE', 'AND', 'OR', 'NOT',
                 'TRUE', 'FALSE', 'NEQV', 'EQV']

    for op in operators:
        # Fix "X. OP." -> "X .OP."  (dot attached to variable)
        line = re.sub(r'(\w)\.\s+(' + op + r')\.', r'\1 .' + op + '.', line, flags=re.IGNORECASE)
        # Fix ".OP ." -> ".OP."  (space before closing dot)
        line = re.sub(r'\.(' + op + r')\s+\.', r'.\1.', line, flags=re.IGNORECASE)
        # Fix ". OP." -> ".OP."  (space after opening dot)
        line = re.sub(r'\.\s+(' + op + r')\.', r'.\1.', line, flags=re.IGNORECASE)

    return line


def fix_keyword_spacing(line):
    """Fix Fortran 77 keyword-variable run-together for free-form.

    In fixed-form, 'INTEGERI' is parsed as 'INTEGER I' because spaces
    don't matter. In free-form, this must have explicit space.
    Only applies when the keyword+rest looks like a declaration, not
    when 'REALS' is just a variable name like REALS(1).
    """
    if line.lstrip().startswith('!'):
        return line

    # Type declarations glued to first variable name
    # Only match if what follows the keyword is a valid variable declaration
    # pattern: keyword followed by uppercase letter that starts a variable name
    # followed by comma or more variable chars (not parenthesized assignment)
    stripped = line.lstrip()

    # INTEGER followed by letter and then comma-separated variables
    # e.g. INTEGERI,ISPC -> INTEGER I,ISPC
    # But NOT INTEGERS(1) = ... (that's a variable)
    keywords = ['INTEGER', 'LOGICAL', 'COMPLEX', 'CHARACTER']
    for kw in keywords:
        if stripped.upper().startswith(kw) and len(stripped) > len(kw):
            rest_start = stripped[len(kw)]
            # Only insert space if next char is a letter and
            # the whole token (keyword+letter...) is immediately followed
            # by a comma, space, or end-of-line (declaration pattern)
            if rest_start.isalpha():
                # Extract what follows
                after_kw = stripped[len(kw):]
                # Check: is this a declaration? Look for comma or just
                # simple variable names
                # Exclude cases like REALS(1) = ... (assignment to array)
                if re.match(r'[A-Za-z]\w*\s*[,]', after_kw) or \
                   re.match(r'[A-Za-z]\w*\s*$', after_kw):
                    indent = len(line) - len(stripped)
                    line = line[:indent] + kw + ' ' + after_kw
                    break

    # REAL is special because 'REALS', 'REALPART' are valid variable names
    # Only fix if followed by a letter then comma (like REALI,J,K)
    if stripped.upper().startswith('REAL') and len(stripped) > 4:
        c = stripped[4]
        if c.isalpha() and c.upper() not in ('*',):
            after = stripped[4:]
            # Must look like a declaration list, not a variable name
            if re.match(r'[A-Za-z]\w*\s*,', after):
                indent = len(line) - len(stripped)
                line = line[:indent] + 'REAL ' + after

    return line


def fix_split_operators(lines):
    """Fix .EQ./.NE./etc. operators split across continuation lines.

    Example: 'IFO. &' then 'EQ.12.OR.' should become 'IFO .EQ. &' then '12.OR.'
    Also: '12. &' then 'OR.IFO' should become '12 .OR. &' then 'IFO'
    """
    operators = ['EQ', 'NE', 'GT', 'GE', 'LT', 'LE', 'AND', 'OR', 'NOT']
    result = list(lines)

    for i in range(len(result) - 1):
        line = result[i].rstrip()
        if not line.endswith('&'):
            continue

        # Find next non-blank line
        j = i + 1
        while j < len(result) and not result[j].strip():
            j += 1
        if j >= len(result):
            continue

        next_line = result[j]
        next_stripped = next_line.lstrip()

        for op in operators:
            # Pattern 1: line ends with 'VARNAME. &' and next starts with 'OP.digits'
            # e.g., 'IFO. &' then 'EQ.12.OR.'
            pat1 = re.compile(r'^(' + op + r')\.(.*)', re.IGNORECASE)
            m1 = pat1.match(next_stripped)
            if m1 and re.search(r'\w\.\s*&\s*$', line):
                # Move the .OP. onto the previous line
                code_before = line.rstrip('& ').rstrip()
                # Remove the trailing dot from the identifier
                if code_before.endswith('.'):
                    code_before = code_before[:-1]
                rest = m1.group(2)
                # Rebuild: put .OP. on previous line
                result[i] = code_before + ' .' + m1.group(1) + '. &\n'
                # Put remaining content on next line with same indent
                indent = len(next_line) - len(next_line.lstrip())
                result[j] = next_line[:indent] + rest + '\n'
                break

            # Pattern 2: line ends with 'number. &' and next starts with 'OP.VARNAME'
            # e.g., '12. &' then 'OR.IFO'
            # This pattern: the '.' after the number is actually the start of '.OR.'
            if re.search(r'\d\.\s*&\s*$', line):
                m2 = pat1.match(next_stripped)
                if m2:
                    code_before = line.rstrip('& ').rstrip()
                    if code_before.endswith('.'):
                        code_before = code_before[:-1]
                    rest = m2.group(2)
                    result[i] = code_before + ' .' + m2.group(1) + '. &\n'
                    indent = len(next_line) - len(next_line.lstrip())
                    result[j] = next_line[:indent] + rest + '\n'
                    break

    return result


def convert_include(line):
    """Convert INCLUDE filenames: .F77 -> .f90, .F -> .f90"""
    match = re.match(r"(\s*INCLUDE\s+['\"])([^'\"]+)(['\"])", line, re.IGNORECASE)
    if not match:
        return line
    prefix, filename, suffix = match.group(1), match.group(2), match.group(3)
    if filename.upper().endswith('.F77'):
        filename = filename[:-4] + '.f90'
    elif filename.upper().endswith('.F'):
        filename = filename[:-2] + '.f90'
    return prefix + filename + suffix


def add_trailing_ampersand(line):
    """Add trailing & to a line, placing it BEFORE any inline comment.

    In free-form Fortran, ! starts a comment to end of line, so & must
    appear before the ! to be recognized as a continuation marker.
    """
    stripped = line.rstrip()
    if not stripped:
        return '&'

    # Find inline comment (! not inside a string)
    in_single = False
    in_double = False
    comment_pos = -1
    for idx, ch in enumerate(stripped):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == '!' and not in_single and not in_double:
            comment_pos = idx
            break

    if comment_pos > 0:
        code_part = stripped[:comment_pos].rstrip()
        comment_part = stripped[comment_pos:]
        return code_part + ' &  ' + comment_part
    else:
        return stripped + ' &'


def convert_file(input_path, output_path):
    """Convert a single fixed-form file to free-form."""
    with open(input_path, 'r', encoding='utf-8', errors='replace') as f:
        original_lines = f.readlines()

    # Strip line endings and expand tabs to spaces (Fortran 77 uses 8-space tabs)
    # Tab expansion is critical: tabs in columns 1-6 can cause misidentification
    # of continuation markers
    original_lines = [l.rstrip('\n').rstrip('\r').expandtabs(8) for l in original_lines]

    converted = []
    i = 0
    n = len(original_lines)

    while i < n:
        line = original_lines[i]

        # Handle blank lines
        if is_blank_line(line):
            converted.append('')
            i += 1
            continue

        # Handle comment lines
        if is_comment_line(line):
            # Convert C/c/* in col 1 to !
            converted.append('!' + line[1:].rstrip())
            i += 1
            continue

        # Handle continuation line (column 6 non-blank on THIS line)
        # This means this line continues the PREVIOUS statement
        if is_continuation_line(line):
            # This shouldn't happen if we process correctly (we consume
            # continuations in the lookahead below). But just in case:
            code = get_code(line)
            if code:
                converted.append('   ' + code)
            i += 1
            continue

        # Regular code line - extract label and code
        label = get_label(line)
        code = get_code(line)

        # Build the converted line
        if label:
            out_line = label + ' ' + code
        else:
            out_line = code

        # Look ahead: does the NEXT non-comment, non-blank line have a
        # continuation marker? If so, this line needs trailing &
        j = i + 1
        while j < n:
            next_line = original_lines[j]
            if is_blank_line(next_line):
                # In fixed-form, blank lines don't break continuation chains
                # but they're unusual between continuations. Keep checking.
                j += 1
                continue
            if is_comment_line(next_line):
                # Comments between continuation lines are possible in fixed-form
                j += 1
                continue
            # Found a non-blank, non-comment line
            if is_continuation_line(next_line):
                # Check if the continuation line is empty (just col 6 marker)
                cont_code_peek = get_code(next_line)
                if not cont_code_peek.strip():
                    # Empty continuation line - skip it entirely
                    # Don't add & to current line
                    converted.append(out_line)
                    i = j + 1
                    break

                # Next code line IS a continuation - we need trailing &
                converted.append(add_trailing_ampersand(out_line))

                # Now consume all continuation lines
                i = j
                while i < n:
                    cline = original_lines[i]

                    if is_blank_line(cline):
                        # Blank line inside continuation block - preserve it
                        converted.append('')
                        i += 1
                        continue

                    if is_comment_line(cline):
                        # Comment inside continuation block
                        converted.append('!' + cline[1:].rstrip())
                        i += 1
                        continue

                    if not is_continuation_line(cline):
                        # No longer a continuation - we're done
                        break

                    # This IS a continuation line - extract its code
                    cont_code = get_code(cline)

                    # Skip empty continuation lines
                    if not cont_code.strip():
                        i += 1
                        continue

                    # Check if the NEXT line is also a continuation
                    k = i + 1
                    next_is_cont = False
                    next_cont_empty = False
                    while k < n:
                        nxt = original_lines[k]
                        if is_blank_line(nxt) or is_comment_line(nxt):
                            k += 1
                            continue
                        if is_continuation_line(nxt):
                            next_is_cont = True
                            # Check if that continuation is empty
                            next_cont_empty = not get_code(nxt).strip()
                        break

                    if next_is_cont and not next_cont_empty:
                        # More real continuations follow - add trailing &
                        converted.append(add_trailing_ampersand('   ' + cont_code))
                    else:
                        # Last continuation line (or next is empty) - no trailing &
                        converted.append('   ' + cont_code)

                    i += 1

                # Don't increment i again at the bottom (already positioned)
                break
            else:
                # Next code line is NOT a continuation - regular line
                converted.append(out_line)
                i += 1
                break
        else:
            # Reached end of file
            converted.append(out_line)
            i += 1

    # Post-process: fix Fortran 77 space-insensitivity issues
    # First fix split operators across continuation lines
    converted = fix_split_operators(converted)

    # Clean up any trailing newlines from fix_split_operators
    converted = [l.rstrip('\n') for l in converted]

    final = []
    for line in converted:
        # Fix operator spacing (Fortran 77 space insensitivity)
        line = fix_operator_spacing(line)
        # Fix keyword-variable run-together
        line = fix_keyword_spacing(line)
        # Fix INCLUDE filenames
        if 'INCLUDE' in line.upper() and not line.lstrip().startswith('!'):
            line = convert_include(line)
        final.append(line)

    # Write output
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(final) + '\n')

    return True


def convert_directory(input_dir, output_dir):
    """Convert all Fortran files in a directory tree."""
    input_dir = Path(input_dir)
    output_dir = Path(output_dir)

    # Find all Fortran source files
    extensions = ('.f', '.F', '.f77', '.F77')
    fortran_files = []
    for ext in extensions:
        fortran_files.extend(input_dir.glob(f'**/*{ext}'))

    # Deduplicate (some might match both .f and .F on case-insensitive FS)
    fortran_files = sorted(set(fortran_files))

    print(f"Found {len(fortran_files)} Fortran files to convert")

    converted_count = 0
    error_count = 0

    for fpath in fortran_files:
        rel = fpath.relative_to(input_dir)
        out_path = output_dir / rel.with_suffix('.f90')

        try:
            convert_file(fpath, out_path)
            converted_count += 1
        except Exception as e:
            print(f"  ERROR: {rel}: {e}")
            error_count += 1

    print(f"Converted: {converted_count}, Errors: {error_count}")
    return converted_count, error_count


if __name__ == '__main__':
    if len(sys.argv) == 3:
        # Single file mode
        inp, out = sys.argv[1], sys.argv[2]
        convert_file(Path(inp), Path(out))
        print(f"Converted {inp} -> {out}")
    elif len(sys.argv) == 4 and sys.argv[1] == '--dir':
        # Directory mode
        convert_directory(sys.argv[2], sys.argv[3])
    else:
        print("Usage:")
        print("  Single file: python fixed_to_free_v2.py input.f output.f90")
        print("  Directory:   python fixed_to_free_v2.py --dir input_dir output_dir")
        sys.exit(1)
