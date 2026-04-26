#!/usr/bin/env python3
"""
Repair F77->F90 conversion bug where C-prefixed comments got mangled
into trailing-& continuations of preceding statements.

Pattern:
    line N:   <statement> &
    line N+1: <whitespace><word fragment>...

Fix:
    line N:   <statement>
    line N+1: ! <word fragment>...   (best-effort comment recovery)

Heuristic: line N+1 starts with 3+ spaces followed by a 2-5 char
lowercase fragment then space then a real word. Such fragments are
almost certainly the tail of a stripped F77 'C  Comment' line.
Leave alone any continuation that looks like real Fortran (assignment,
operator, function call, etc.).
"""
import re
import sys
from pathlib import Path

# A line ending in optional whitespace + & + optional whitespace is a continuation
TRAILING_AMP = re.compile(r"^(.*?)\s&\s*$")

# A line that looks like a stripped-C comment fragment:
#   indent, lowercase 2-5 chars, space, then more text
FRAGMENT = re.compile(r"^(\s+)([a-z]{2,5})\s+(\S.*)$")

# Real Fortran continuations have things like "var =", function calls,
# operators, parentheses. Reject these patterns.
LOOKS_LIKE_CODE = re.compile(
    r"[=()\[\]<>+\-*/]|\.eq\.|\.lt\.|\.gt\.|\.ne\.|\.and\.|\.or\.|\.not\."
)


def repair(path: Path) -> int:
    src = path.read_text().splitlines(keepends=True)
    out = []
    i = 0
    fixed = 0
    while i < len(src):
        line = src[i]
        m_amp = TRAILING_AMP.match(line.rstrip("\n"))
        if m_amp and i + 1 < len(src):
            nxt = src[i + 1]
            m_frag = FRAGMENT.match(nxt.rstrip("\n"))
            if m_frag and not LOOKS_LIKE_CODE.search(nxt):
                # Repair: drop the trailing & on line N, convert N+1 to comment
                out.append(m_amp.group(1).rstrip() + "\n")
                indent = m_frag.group(1)
                rest = m_frag.group(2) + " " + m_frag.group(3)
                out.append(indent + "! " + rest + "\n")
                i += 2
                fixed += 1
                continue
        out.append(line)
        i += 1
    if fixed:
        path.write_text("".join(out))
    return fixed


if __name__ == "__main__":
    targets = sys.argv[1:] or [
        "src-converted/econ/eccalc.f90",
        "src-converted/econ/echarv.f90",
        "src-converted/econ/ecin.f90",
        "src-converted/econ/ecsetp.f90",
        "src-converted/econ/ecstatus.f90",
        "src-converted/econ/ecvol.f90",
        "src-converted/econ/ecinit.f90",
    ]
    total = 0
    for t in targets:
        n = repair(Path(t))
        print(f"{t}: {n} lines repaired")
        total += n
    print(f"Total: {total}")
