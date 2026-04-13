# ACD Cardinal Diagnostic Handoff

Purpose: reconcile the discrepancy between the clean workspace build of
the ACD variant and the reported failures on cardinal.osc.edu so the
ACD status can be finalized for the `v2026.04.0` tag.

## Context

The workspace copy of `src-converted/acd/` compiles cleanly with
gfortran 15.2: all 135 `MAXSP`-declared arrays in common blocks match
their DATA statement counts (108 values each), and `blkdat.f90` and
`crown.f90` both exit 0 with only the expected REAL-to-INTEGER
conversion warnings. The bug Aaron saw on Cardinal must therefore be
a property of the Cardinal environment, not the source.

## Hypotheses in priority order

1. **Stale or wrong `PRGPRM.f90` on Cardinal CMake include path.** The
   CMake build may resolve to a different PRGPRM header (for instance,
   the NE variant's 108-species header leaking through, or an ACD
   header left at MAXSP=107 from a prior dev cycle). The diagnostic
   script captures `acd/PRGPRM.f90` and any PRGPRM.f90 reachable via
   the Cardinal module load path.
2. **ifort strictness.** Cardinal's default Fortran compiler is ifort,
   not gfortran. ifort flags DATA/common-block mismatches as hard
   errors where gfortran 15.2 warns. The diagnostic records the exact
   compiler version and flags the CMake build invokes.
3. **Out-of-date local clone.** The Cardinal copy at
   `/users/PUOM0008/crsfaaron` may be behind the workspace. The
   diagnostic captures `git describe`, `git log -1`, and a diff
   against origin/main for `acd/blkdat.f90` and `acd/crown.f90`.

## What the diagnostic produces

`deployment/scripts/diagnose_acd_cardinal.sh` writes a single log
containing source hashes, PRGPRM resolution, compiler version, CMake
flags, and the first 200 lines of stdout/stderr from an isolated ACD
build. Run it on Cardinal and send the log back.

## Decision tree after running

- If hashes match workspace and build succeeds: the Cardinal issue was
  a stale clone or transient env. Update `KNOWN_ISSUES.md` to promote
  ACD to supported.
- If hashes match but build fails with ifort: capture the ifort error,
  add an entry to `KNOWN_ISSUES.md` noting ACD ships with gfortran
  only, and keep ACD advisory for this tag.
- If hashes differ: resync Cardinal to origin/main and rerun. A
  divergent Cardinal copy is the simplest fix.
- If MAXSP is genuinely wrong in the Cardinal copy: do not trim species
  data (it breaks model coefficients). Increase MAXSP in
  `acd/PRGPRM.f90` and add the matching tail value to every DATA
  statement. Document in the commit message.
