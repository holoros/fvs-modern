# NSBE Coefficient Data: Deployment to Cardinal HPC

This document describes how to deploy the NSBE/VTECO coefficient data to Cardinal or other HPC systems.

## Quick Summary

The NSBE volume and biomass equation system has been integrated into the FVS calibration project. All required data files are now available in the local project directory and can be copied to Cardinal.

## Files to Deploy

All files are located in `/sessions/kind-upbeat-darwin/mnt/Claude/fvs-modern/data/NSBE/` and should be copied to the same relative location on Cardinal.

### File Manifest

```
data/NSBE/
├── README.md                                    (361 KB) - Documentation
├── REF_SPECIES.csv                              (125 KB) - Species reference table
└── Coefs/combined/
    ├── volib_coefs.csv                         (35 KB)  - Volume inside-bark
    ├── volob_coefs.csv                         (23 KB)  - Volume outside-bark
    ├── volbk_coefs.csv                         (29 KB)  - Bark volume (optional)
    ├── total_biomass_coefs.csv                 (15 KB)  - Total aboveground biomass
    ├── bark_biomass_coefs.csv                  (16 KB)  - Bark component
    ├── branch_biomass_coefs.csv                (13 KB)  - Branch component
    ├── foliage_coefs.csv                       (17 KB)  - Foliage (optional)
    ├── rcumib_coefs.csv                        (18 KB)  - Merchantable vol IB (optional)
    └── rcumob_coefs.csv                        (16 KB)  - Merchantable vol OB (optional)
└── Files/
    └── equation_forms_and_calls.csv             (1 KB)  - Equation form definitions
```

**Total size**: ~188 KB (highly compressible, < 50 KB when gzipped)

## Deployment Methods

### Method 1: SCP (Recommended)

Copy directly from development machine to Cardinal:

```bash
# From /sessions/kind-upbeat-darwin/mnt/Claude/ on your machine:
scp -r fvs-modern/data/NSBE user@cardinal.hpc.nd.edu:/path/to/fvs-modern/data/

# Or create tarball for efficiency:
cd fvs-modern
tar czf NSBE.tar.gz data/NSBE/
scp NSBE.tar.gz user@cardinal.hpc.nd.edu:/path/to/fvs-modern/
# Then on Cardinal:
cd /path/to/fvs-modern
tar xzf NSBE.tar.gz
```

### Method 2: rsync (For Updates)

```bash
rsync -avz --delete fvs-modern/data/NSBE/ \
  user@cardinal.hpc.nd.edu:/path/to/fvs-modern/data/NSBE/
```

### Method 3: Git (If Repository)

If fvs-modern is a git repository, add the data directory:

```bash
cd fvs-modern
git add data/NSBE/
git commit -m "Add NSBE coefficient data (Westfall et al. 2024)"
git push
# Then on Cardinal: git pull
```

## Environment Setup on Cardinal

After copying the data, two options for R scripts to find the coefficients:

### Option A: Project-relative Path (Recommended)

No additional setup needed. The R script (20_volume_equations.R) automatically looks in:
```
{FVS_PROJECT_ROOT}/data/NSBE
```

Just ensure `FVS_PROJECT_ROOT` is set correctly when running scripts.

### Option B: Environment Variable

Set on Cardinal:
```bash
export NSBE_ROOT=/path/to/fvs-modern/data/NSBE
```

Then in job scripts or .bashrc:
```bash
export NSBE_ROOT=/work/cardinal/user/fvs-modern/data/NSBE
```

## Verification

After copying, verify the data is intact:

```bash
# On Cardinal, check file presence and sizes
ls -lh data/NSBE/Coefs/combined/
ls -lh data/NSBE/Files/

# Check record counts in key files
wc -l data/NSBE/Coefs/combined/volib_coefs.csv
wc -l data/NSBE/REF_SPECIES.csv
```

## Integration with R Scripts

The volume equations script (calibration/R/20_volume_equations.R) automatically:

1. Searches for NSBE data in this order:
   - `$NSBE_ROOT` environment variable
   - `{FVS_PROJECT_ROOT}/data/NSBE`
   - `/home/aweiskittel/Documents/MAINE/DATA/Analysis/NSBE/VTECO_modified` (fallback)

2. Loads all coefficient files on first call to `compute_nsbe_volume()` or `compute_stand_volume_nsbe()`

3. Caches coefficients in memory for performance

## Cardinal Job Script Example

```bash
#!/bin/bash
#SBATCH --job-name=fvs_volume
#SBATCH --time=01:00:00
#SBATCH --nodes=1
#SBATCH --mem=8G

# Load modules
module load r/4.3.0

# Set environment
export FVS_PROJECT_ROOT=/work/cardinal/user/fvs-modern
export NSBE_ROOT=$FVS_PROJECT_ROOT/data/NSBE

# Run R script
cd $FVS_PROJECT_ROOT
Rscript calibration/R/20_volume_equations.R
```

## Troubleshooting

### "Cannot locate NSBE data" Error

This means the script can't find the coefficients. Check:

```bash
# 1. Data files exist
ls -l $FVS_PROJECT_ROOT/data/NSBE/Coefs/combined/volib_coefs.csv

# 2. Environment variable is set (if using Method B)
echo $NSBE_ROOT

# 3. FVS_PROJECT_ROOT is correct
echo $FVS_PROJECT_ROOT
```

### File Permission Issues

On Cardinal, ensure files are readable:

```bash
chmod -R 644 data/NSBE/
chmod -R 755 data/NSBE/Coefs data/NSBE/Files
```

### Data Integrity Check

Verify CSV files after copying:

```bash
# Check first few lines
head -5 data/NSBE/Coefs/combined/volib_coefs.csv

# Count lines
wc -l data/NSBE/Coefs/combined/volib_coefs.csv
```

## Backup and Archival

The NSBE coefficient files are static and represent a specific version (VTECO_modified, 2024). It is recommended to:

1. Keep a copy in version control (if applicable)
2. Archive a snapshot on long-term storage
3. Document the version date (2024) for reproducibility

## Size and Performance Notes

- **Total uncompressed size**: ~188 KB (negligible on modern systems)
- **Load time**: ~100-500 ms per R session (one-time)
- **Memory footprint**: ~2 MB in R memory (all tables cached)
- **Network transfer**: < 50 KB gzipped (trivial)

No performance concerns for typical HPC workflows.

## Updates and Maintenance

If NSBE coefficients are updated (e.g., new species or ecodivisions):

1. Update source in: `/home/aweiskittel/Documents/MAINE/DATA/Analysis/NSBE/VTECO_modified`
2. Copy to: `/sessions/kind-upbeat-darwin/mnt/Claude/fvs-modern/data/NSBE/`
3. Document change in this file
4. Push to Cardinal using method above (SCP, rsync, or git)

---

**Deployment Date**: 2026-04-04
**NSBE Version**: VTECO_modified (2024)
**Status**: Ready for production deployment
