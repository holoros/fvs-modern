# fvs-modern Update Lifecycle

## Overview

The USFS publishes FVS updates roughly 3 times per year (quarterly, with Q2 sometimes skipped). Updates typically include bug fixes, new species coefficients, volume equation corrections, and occasional new features. This document describes how fvs-modern tracks, integrates, and verifies upstream changes.

## Automated Monitoring

A scheduled task (`fvs-upstream-scan`) runs on the 1st and 15th of each month at 9:00 AM. It queries the GitHub API for both USFS repositories:

* `USDAForestService/ForestVegetationSimulator` (Fortran source)
* `USDAForestService/ForestVegetationSimulator-Interface` (rFVS + fvsOL)

The task generates a change report at `docs/upstream_report.md` and notifies you if new commits are detected. No manual action is needed for monitoring.

## Update Workflow

When new upstream commits are detected, follow this sequence:

### Step 1: Review the change report

Read the upstream report to understand what changed. Key things to look for:

* Bug fixes in `vbase/` or `base/` that overlap with our patches (FORMAT fixes in `initre.f`, computed GOTO conversions in `algevl.f`, `evldx.f`, `errgro.f`, `main.f`, `myopen.f`, `volkey.f`)
* New species coefficients or variant-specific changes
* Interface changes that affect the NAMESPACE or DESCRIPTION

### Step 2: Pull and integrate

```bash
cd /path/to/fvs-modern/deployment/scripts

# Dry run first to see what would change
./sync_upstream.sh --pull --dry-run

# Full integration: pull, apply patches, convert, rebuild, test
./sync_upstream.sh --all --fvs-src /path/to/ForestVegetationSimulator-main
```

The sync script automatically:
* Backs up the current source before overwriting
* Downloads the latest USFS source from GitHub
* Applies fvs-modern patches (FORMAT fixes, etc.)
* Converts changed .f files to .f90 free form
* Rebuilds all 24 variant shared libraries (22 US + 2 Canadian)
* Runs the full regression test suite

### Step 3: Handle merge conflicts

If USFS has modified files we've also patched, the sync script will overwrite with their version, then re-apply our patches. If their changes overlap with our FORMAT fixes, you may need to manually verify:

```bash
# Check if our patched files were changed upstream
diff /path/to/backup/vbase/initre.f /path/to/new/vbase/initre.f
```

Our FORMAT fixes (`A8'` -> `A8,'`) are safe to re-apply blindly since they correct syntax errors that gfortran catches but Intel Fortran ignores.

### Step 4: Verify

```bash
# Run the full regression suite
./run_regression_tests.sh /path/to/bin /path/to/tests

# Expected: 64/65 pass (iet03 segfault is a known issue)
# New failures indicate upstream regressions or patch conflicts
```

### Step 5: Update baseline

After successful integration, update the sync state:

```bash
# Edit ~/.fvs-modern/upstream_sync.json
# Set fvs_last_date and iface_last_date to today
```

## Creating New Variants

Use `add_variant.sh` to scaffold a new regional variant from an existing one:

```bash
./add_variant.sh --name me --base ne --description "Maine variant with Acadian calibration"
```

This creates the directory structure, species mapping template, and calibration parameter files. The process for building a calibrated variant:

1. Define species list in `calibration/species_map.csv`
2. Update `PRGPRM.f` with the correct `MAXSP`
3. Fit regional height-diameter, site index, and mortality models (see Weiskittel et al. 2011)
4. Encode coefficients in the variant source files (`htgf.f`, `dgf.f`, `morts.f`, etc.)
5. Build: `OSTYPE=linux-gnu make FVSme.so`
6. Test: `R -e "library(rFVS); fvsLoad('FVSme', bin='path/to/bin')"`

## Model Calibration Without a New Variant

For lighter calibration (adjusting existing variant coefficients without creating a new variant), FVS supports runtime calibration through keyword files:

* `READCORR` keyword: applies species-specific multipliers to height and diameter growth
* `MORTMULT` keyword: adjusts mortality rates by species
* `SITEINDEX` keyword: overrides default site index curves
* `BFVOLUME`/`VOLUME` keywords: adjusts volume equation parameters

These can be embedded in keyword files passed to fvsOL without modifying any Fortran source.

## Docker/Server Updates

After integrating upstream changes locally:

```bash
# Rebuild Docker image with updated source
cd deployment/docker
docker compose build --no-cache

# Or for the Ubuntu server (charcoal):
# Copy updated .so files and reinstall R packages
scp /path/to/bin/FVS*.so user@charcoal:/opt/fvs/bin/
ssh user@charcoal "sudo systemctl restart fvsonline"
```

## Timeline of USFS Release Cadence (recent)

* 2025-04: Q2 2025 (activity capacity expansion, measurement period decimals)
* 2025-05: Q3 2025 (consolidation)
* 2025-08: FIA VBC updates, Midgard interface PR
* 2025-09: Q3 staging merge (both repos)
* 2025-10: Include casing fix
* 2025-11: Staging consolidation (latest as of March 2026)

## Files Tracked by fvs-modern Patches

These files have fvs-modern specific modifications. Any upstream changes to these files require manual review during sync:

| File | Patch Type | Description |
|------|-----------|-------------|
| vbase/initre.f | FORMAT fix | 11 missing commas in FORMAT descriptors |
| base/main.f90 | SELECT CASE | Computed GOTO -> SELECT CASE (STOP codes) |
| base/myopen.f90 | SELECT CASE | Computed GOTO -> SELECT CASE (file status) |
| base/volkey.f90 | SELECT CASE | Computed GOTO -> SELECT CASE (volume keywords) |
| base/algevl.f90 | SELECT CASE | 3 computed GOTOs -> SELECT CASE (operators) |
| base/evldx.f90 | SELECT CASE | 3 computed GOTOs -> SELECT CASE (attributes) |
| base/errgro.f90 | SELECT CASE | Computed GOTO -> SELECT CASE (error routing) |
