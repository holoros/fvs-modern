# FVS Calibration: Scripts 20 and 21 Documentation

## Script 20: Volume Equations (20_volume_equations.R)

**Purpose:** Adds total cubic foot and board foot volume computation to the stand projection engine using the FIA Component Ratio Method (CRM) approach and Jenkins et al. (2003) biomass equations.

**Key Functions:**

- `compute_stand_volume(trees, variant = NULL)`
  - **Input:** Tree list with columns: dbh (inches), ht (feet), tpa (trees per acre), spcd (species code)
  - **Output:** List with 4 components:
    - `cuft_per_acre`: Total cubic foot volume (green wood)
    - `bdft_per_acre`: Board foot volume per acre (International 1/4" rule, sawtimber >= 9.0" DBH only)
    - `biomass_tons_per_acre`: Above-ground dry biomass in short tons
    - `carbon_tons_per_acre`: Carbon mass (biomass * 0.5)
  - **Coefficients Used:**
    - Cubic foot: V = b0 * DBH^b1 * HT^b2 (Woudenberg et al. 2010)
    - Board foot: V = b0 + b1*DBH^2*HT (Allen & Campbell 2002)
    - Biomass: Dry biomass (kg) = a * DBH_cm^b (Jenkins et al. 2003)
  - **Species Classification:** Automatic softwood/hardwood mapping; 60+ common FVS species codes included
  - **Regional Support:** Separate coefficients for NE, NC, SO, PN, CA, SW regions

- `add_volume_to_trajectory(trajectory, trees_trajectory, variant = NULL)`
  - Wrapper to add volume columns to projection output from simulate_stand_v2()
  - Returns trajectory with added columns: cuft_per_acre, bdft_per_acre, biomass_tons_per_acre, carbon_tons_per_acre

- `simulate_stand_v2_with_trees(...)`
  - Enhanced version of simulate_stand_v2() that also returns tree lists by year
  - Enables retroactive volume computation

**Coefficient Sources:**
- **Cubic Foot:** Woudenberg et al. (2010). Forest Inventory and Analysis Database Description and Users Manual (v4.01)
- **Board Foot:** Allen, E.A., and S.G. Campbell. 2002. Volume equations for northern California tree species
- **Biomass:** Jenkins, J.C., D.C. Chojnacky, L.S. Heath, and R.A. Birdsey. 2003. National-scale biomass estimators for United States tree species. Forest Science 49(1): 12-35

**Usage Example:**
```r
source("calibration/R/20_volume_equations.R")
trees <- tibble(dbh = c(10, 15, 20), ht = c(60, 80, 100), 
                tpa = c(100, 80, 60), spcd = c(97, 97, 316))
volumes <- compute_stand_volume(trees, variant = "ne")
```

**Regional Mapping:**
- NE: acd, ne, on
- NC: ls, cs, nc, ec, ks
- SO: oc, so, tt, em, op
- PN: pn, bc, ak, wc
- CA: ca, ci
- SW: ws, ut, bm, sn, cr, ie, kt

---

## Script 21: Uncertainty Propagation via Posterior Draws (21_uncertainty_propagation.R)

**Purpose:** Propagates parameter uncertainty through stand projections by sampling from posterior distributions of calibrated models (diameter growth, mortality, height-diameter). Produces trajectory envelopes with credible intervals.

**Key Functions:**

- `load_posterior_draws(variant, n_draws = 200, seed = 123)`
  - Loads and samples posterior parameter draws from .rds files for a given variant
  - Returns list with hd_samples, dg_samples, mort_samples, dg_std, sdimax, meas_interval

- `extract_draw_params(posterior_list, draw_index)`
  - Extracts the draw_index-th parameter set from posterior samples
  - Converts to format compatible with simulate_stand_v2()

- `simulate_stand_v2(trees, params, si, years, ...)`
  - Standard projection engine (copied from script 17 for self-containment)
  - Supports both calibrated and default parameter modes

**Workflow:**

1. For each variant:
   - Load posterior samples from output/variants/{variant}/*.rds files
   - Sample up to n_draws parameter sets (default 200)
   - Run projection for each draw with identical initial conditions
   - Pool trajectories and compute percentile-based credible intervals

2. Credible intervals computed:
   - 2.5th percentile (lower 95% CI)
   - 25th percentile (lower 50% CI)
   - 50th percentile (median)
   - 75th percentile (upper 50% CI)
   - 97.5th percentile (upper 95% CI)

**Output Files:**

- `output/comparisons/stand_projections_with_ci.csv`
  - One row per year per variant
  - Columns: variant, region, year, ba_q025, ba_q25, ba_q50, ba_q75, ba_q975, tpa_q50, qmd_q50

- `output/manuscript_tables/uncertainty_summary_year50.csv`
  - Year 50 summary: median BA, 95% CI bounds, CI width (absolute and percent)
  - Sorted by relative uncertainty (CI width as % of median)

**Figure Generation:**

1. `fig_ci_by_region.png`
   - Faceted by region (8 panels)
   - Shows 50% and 95% credible intervals as ribbons
   - Median as solid line

2. `fig_ci_all_variants.png`
   - All variants on single plot
   - Colored by region
   - 95% CI envelopes as semi-transparent ribbons

3. `fig_calibrated_vs_default_with_ci.png`
   - Comparison of calibrated (with CI) vs default FVS-like
   - Faceted by region
   - Requires output from script 17

4. `fig_trajectory_envelope_ne.png`
   - All 200 posterior draws plotted as thin lines
   - Quantile curves overlaid (50th, 2.5th, 97.5th percentiles)
   - Representative variant: Northeast (NE)

**Parallel Execution:**

- Uses `future` and `future.apply` for efficient parallelization
- Default: 4 workers per variant (auto-detected available cores)
- Each draw runs complete 50-year projection independently

**Dependencies:**

- Posterior .rds files must exist in output/variants/{variant}/:
  - height_diameter_samples.rds
  - diameter_growth_samples.rds
  - mortality_samples.rds
  - standardization_params.rds
- Optional: species_sdimax_calibrated.csv for density regulation

**Parameters:**
- `n_draws`: Number of posterior samples to draw per variant (default 200)
- `years`: Projection length (default 50)
- `si`: Site index for initial stand (default 65)
- Initial stand: 200 TPA, ~6 in. QMD, randomized softwood/hardwood mix

---

## Integration with Script 17

Both scripts are self-contained and designed to run after script 17 (stand_projection_engine.R):

- **Script 17 Output:** stand_projections_3scenarios.csv (required by script 21 for comparison figure)
- **Script 20:** Adds volume-to-yield computation, integrates with projection output
- **Script 21:** Uses posterior files generated during script 17 calibration workflow

## Running on Cardinal

Both scripts use environment variable FVS_PROJECT_ROOT for portability:

```bash
export FVS_PROJECT_ROOT=/path/to/fvs-modern
Rscript calibration/R/20_volume_equations.R
Rscript calibration/R/21_uncertainty_propagation.R
```

No hyphens in axis labels per user preference; all labels use underscores or spaces.
Uses tidyverse and ggplot2 with theme_minimal.

## References

- Woudenberg, S.W., B.L. Conkling, J.A. O'Connell, E.B. LaPoint, and K.L. Turner. 2010. The Forest Inventory and Analysis Database: Database Description and Users Manual. General Technical Report RMRS-GTR-245-www. Fort Collins, CO: U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station. 258 p.

- Jenkins, J.C., D.C. Chojnacky, L.S. Heath, and R.A. Birdsey. 2003. National-scale biomass estimators for United States tree species. Forest Science 49(1): 12-35.

- Allen, E.A., and S.G. Campbell. 2002. Landowner Guide to Timber Management in Northern California. UC Davis Cooperative Extension.

