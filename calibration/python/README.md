# calibration/python

Python projection engines and post-processing scripts for the FVS Bayesian calibration pipeline.

## Scripts

### perseus_100yr_projection.py

Base FVS projection engine for PERSEUS plots.

Runs FVS-ACD and FVS-NE (both default and calibrated parameters) for 3,632 Maine PERSEUS plots over 100 years using the fvs2py shared library. Extracts NSBE total aboveground biomass at each 10-year step.

Pipeline:
1. Read PERSEUS CSV to retrieve FIRST_PLTCN for each plot
2. Load FIA tree data for those PLT_CNs from state SQLite database
3. For each plot x variant x config combination:
   - Populate FVS standinit and treeinit SQLite tables
   - Generate keyword file (10 cycles of 10 years = 100 years)
   - Run FVS via fvs2py shared library
   - Extract tree lists at each cycle from TREELIDB
   - Apply NSBE biomass equations per tree
   - Compute per-plot aboveground biomass (AGB) from sum(biomass_kg * TPA)
4. Output: CSV with PLOT x YEAR x VARIANT x CONFIG x AGB

Designed for SLURM array jobs on Cardinal HPC.

Usage:

```bash
python perseus_100yr_projection.py --batch-id 1 --batch-size 100
python perseus_100yr_projection.py --all
```

### perseus_uncertainty_projection.py

Extends base projection with Bayesian uncertainty propagation.

Runs 500 posterior draws of FVS for each plot x variant combination. Each draw uses a complete parameter vector sampled from the posterior distribution returned by the calibration pipeline (config/calibrated/{variant}_draws.json). All components within a single draw share the same MCMC iteration index, preserving parameter correlations between growth and mortality.

Output: CSV with PLOT x YEAR x VARIANT x DRAW_INDEX x AGB. Post-processing computes quantiles to produce credible intervals.

Usage:

```bash
python perseus_uncertainty_projection.py --batch-id 1 --n-draws 500 --batch-size 50
```

### perseus_uncertainty_aggregate.py

Post-processing aggregator for Bayesian ensemble results.

Combines batch output from perseus_uncertainty_projection.py into state-level summaries. Computes credible intervals (2.5%, 10%, 50%, 90%, 97.5%) on megatonnes (MMT) of carbon at each projection timestep.

Produces:
- State-level MMT with credible intervals (wide format)
- Tree-list ensemble diagnostics (MCMC effective sample size, convergence metrics)
- Comparison table: ensemble mean vs. calibrated median vs. default estimate

Usage:

```bash
python perseus_uncertainty_aggregate.py output/batch_*.csv
```

### perseus_aggregate.py

Original point-estimate aggregation (uses calibrated parameters only, no uncertainty).

Reads individual plot projections and aggregates to state-level megatonnes carbon. Simpler alternative to uncertainty aggregator when ensemble runs are unavailable.

Usage:

```bash
python perseus_aggregate.py output/plots_100yr.csv
```

### bakuzis_100yr_comparison.py

Bakuzis matrix comparison analysis.

Compares FVS predictions (default and calibrated) against historical growth data from the Bakuzis long-term growth matrix. Computed metrics:

- Mean absolute percentage error (MAPE)
- Bias (systematic over/under prediction)
- Root mean square error (RMSE)

Helps validate calibration quality by checking prediction accuracy on independent data.

Usage:

```bash
python bakuzis_100yr_comparison.py --variant ne
```

## Dependencies

Required packages:

- numpy: Array operations
- pandas: Data frame manipulation and I/O
- fvs2py: Shared library wrapper from deployment/fvs2py (loaded via ctypes)

The fvs2py module is automatically added to sys.path if FVS_PROJECT_ROOT environment variable is set.

Required external data:

- FIA SQLite database (state level, e.g., ME_fvs_input.db)
- PERSEUS plot list CSV
- NSBE biomass coefficients (CSV or R object format)
- Posterior draws JSON (config/calibrated/{variant}_draws.json for uncertainty scripts)

Environment variables:

```bash
FVS_PROJECT_ROOT=/path/to/fvs-modern       # Repo root (auto-detected if not set)
FVS_LIB_DIR=/path/to/fvs-modern/lib        # Location of .so files (default: ./lib)
FVS_FIA_DATA_DIR=/path/to/fia_data         # FIA SQLite directory
NSBE_ROOT=/path/to/NSBE                    # NSBE coefficient directory
```

## Running from Repository Root

All scripts import from calibration/python/ and config/ modules. Run scripts from the repository root:

```bash
cd /path/to/fvs-modern
export FVS_PROJECT_ROOT=$(pwd)
python calibration/python/perseus_100yr_projection.py --all
```

Alternatively, use the provided sbatch templates in calibration/osc/ for Cardinal HPC submission.

## Output Locations

Batch results default to ./calibration/output/batch_{batch_id}.csv.

State-level aggregates default to ./calibration/output/state_mmt_{variant}_{config}.csv.

Diagnostic plots (convergence checks, ensemble distributions) default to ./calibration/output/figures/.

## Module Interdependencies

These scripts form a processing pipeline:

1. perseus_100yr_projection.py or perseus_uncertainty_projection.py produces per-plot results
2. perseus_aggregate.py (point estimate) or perseus_uncertainty_aggregate.py (ensemble) combines batches
3. bakuzis_100yr_comparison.py validates against independent data

The config/ module (config_loader.py, uncertainty.py) is imported by all projection scripts to manage parameter versions and posterior sampling.
