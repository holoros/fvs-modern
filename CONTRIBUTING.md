# Contributing to fvs-modern

Thank you for your interest in improving the Forest Vegetation Simulator. This document outlines how to contribute effectively.

## Getting started

1. Fork the repository and clone your fork
2. Create a feature branch from `main`: `git checkout -b feature/my-improvement`
3. Make your changes and test them
4. Push to your fork and open a pull request

## Development environment

You need:

* gfortran 12+ (or any Fortran compiler supporting `-std=legacy` and `-fallow-argument-mismatch`)
* R 4.2+ with the `rFVS` package dependencies
* Linux, macOS (via Homebrew), or Windows (via WSL2)

Build a single variant for quick testing:

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted /tmp/test-build ie
```

## What we need help with

* **Computed GOTO conversions** (5 remaining in base/vbase, see `grep -rn "GO TO (" src-converted/base/ src-converted/vbase/`)
* **COMMON block to module migration** (large effort, see `docs/modernization_roadmap.md`)
* **Test coverage** for Canadian variants (BC, ON) and individual variant edge cases
* **Documentation** improvements, especially variant specific notes
* **Bug fixes** in upstream USDA code (e.g., the known FVSie/iet03 segfault)

## Code style

* Fortran: free form (.f90), 132 column width, lowercase keywords, use `IMPLICIT NONE` in new routines
* When converting computed GOTOs: use `SELECT CASE` where branches are self contained; keep labeled GOTOs for fall through patterns
* Shell scripts: `set -euo pipefail`, use `#!/usr/bin/env bash`
* R code: tidyverse style, `library()` calls at top of scripts

## Pull request guidelines

* Keep PRs focused on a single concern
* Include a brief description of what changed and why
* If modifying Fortran source, verify with `gfortran -fsyntax-only -ffree-form -std=legacy`
* If modifying the build script, test at least one variant build
* Reference any related issues

## Upstream sync awareness

This fork tracks two USFS repositories. Before modifying files in `src-converted/`, check whether the file is frequently updated upstream (see `docs/update_lifecycle.md`). Changes to heavily updated files may create merge conflicts during upstream syncs.

Files in `base/` and `vbase/` are relatively stable. Variant specific files (e.g., `ie/`, `ne/`) are updated more frequently by USFS.

## Reporting issues

Use GitHub Issues. Please include:

* Which variant(s) are affected
* Steps to reproduce
* Expected vs. actual behavior
* Your platform and compiler version
