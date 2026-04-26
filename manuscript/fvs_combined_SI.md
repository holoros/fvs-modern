---
title: "Supplementary Information"
subtitle: "Modernizing and Bayesian recalibrating the Forest Vegetation Simulator: open source infrastructure and parametric uncertainty quantification for 25 geographic variants"
author:
  - Aaron R. Weiskittel
  - Greg Johnson
  - David Marshall
date: 2026-04-23
---

# SI contents

This supplement expands on the Results and Applications section of the main
manuscript with per variant diagnostics, spatial distributions of
calibration improvement, and component level posterior summaries.
Figures are grouped into four parts:

1. **Per variant predicted vs observed** — tree level calibration
   diagnostics.
2. **Per variant performance** — RMSE, bias, R squared by variant.
3. **Spatial distribution of improvement** — hex binned CONUS maps.
4. **Component level posterior diagnostics** — mortality AUC, diameter
   growth R squared, crown ratio comparison, and the conceptual diagram.

All figures are reproducible from the scripts in calibration/R/ and
calibration/python/; filenames below match the artifacts under
calibration/output/comparisons/manuscript_figures/ in the public
repository.

# Part 1. Per variant predicted vs observed

![Figure SI.1. Predicted vs observed basal area under calibrated parameters, aggregated across all tested variants. Points represent individual FIA remeasurement conditions; the diagonal is the 1:1 line and the solid line is the regression fit.](../calibration/output/comparisons/manuscript_figures/01_ba_pred_vs_obs_calibrated.png){width=6in}

![Figure SI.2. Predicted vs observed basal area under default FVS parameters on the same FIA remeasurement set, for direct comparison with Figure SI.1.](../calibration/output/comparisons/manuscript_figures/02_ba_pred_vs_obs_default.png){width=6in}

![Figure SI.3. Predicted vs observed quadratic mean diameter under calibrated parameters.](../calibration/output/comparisons/manuscript_figures/07_qmd_pred_vs_obs_calibrated.png){width=6in}

![Figure SI.4. Predicted vs observed gross cubic foot volume under calibrated parameters, derived through the combined variable ratio of FIA per tree volumes with projected diameters and heights.](../calibration/output/comparisons/manuscript_figures/08_vol_cfgrs_pred_vs_obs_calibrated.png){width=6in}

![Figure SI.5. Predicted vs observed top height under calibrated parameters.](../calibration/output/comparisons/manuscript_figures/11_ht_top_pred_vs_obs.png){width=6in}

![Figure SI.6. Predicted vs observed periodic annual increment for basal area under calibrated parameters.](../calibration/output/comparisons/manuscript_figures/10_pai_pred_vs_obs.png){width=6in}

![Figure SI.7. Gross versus net periodic annual increment under calibrated parameters, reflecting the underestimation of net increment when ingrowth recruitment is not modeled.](../calibration/output/comparisons/manuscript_figures/12_pai_gross_vs_net.png){width=6in}

![Figure SI.8. Distribution of residuals (predicted minus observed) across all remeasurement conditions under default and calibrated configurations.](../calibration/output/comparisons/manuscript_figures/05_residual_distributions.png){width=6in}

# Part 2. Per variant performance

![Figure SI.9. Basal area RMSE by variant, comparing default and calibrated configurations. Error bars are bootstrap 95 percent confidence intervals on RMSE.](../calibration/output/comparisons/manuscript_figures/14_ba_rmse_bootstrap_ci.png){width=6in}

![Figure SI.10. Basal area R squared by variant with bootstrap confidence intervals for default and calibrated configurations.](../calibration/output/comparisons/manuscript_figures/14b_ba_r2_bootstrap_ci.png){width=6in}

![Figure SI.11. Per variant bias dumbbell plot. Lines connect default (open) and calibrated (filled) bias; a shorter line with the calibrated marker closer to zero indicates greater improvement.](../calibration/output/comparisons/manuscript_figures/04_ba_bias_dumbbell.png){width=6in}

![Figure SI.12. Confidence interval coverage by variant: fraction of FIA remeasurement observations that fall inside the predicted 95 percent credible band under calibrated parameters.](../calibration/output/comparisons/manuscript_figures/15_ci_bracket_coverage.png){width=6in}

![Figure SI.13. Diameter growth calibrated R squared by variant, organized from highest to lowest.](../calibration/output/comparisons/manuscript_figures/fig_dg_r2_by_variant.png){width=6in}

![Figure SI.14. Mortality AUC by variant under calibrated models.](../calibration/output/comparisons/manuscript_figures/fig_mort_auc_by_variant.png){width=6in}

![Figure SI.15. Calibration improvement composite metric by variant, combining BA and volume RMSE reductions into a single score.](../calibration/output/comparisons/manuscript_figures/07_calibration_improvement.png){width=6in}

![Figure SI.16. Basal area equivalence testing results by variant, showing which variants pass the 20 percent practical equivalence threshold.](../calibration/output/comparisons/manuscript_figures/06_ba_equivalence.png){width=6in}

# Part 3. Spatial distribution of improvement

These maps aggregate FIA remeasurement conditions into hex bins across
the contiguous United States and show where calibrated models
outperformed default. Panels cover basal area, volume, win rate (share
of plots where calibrated outperformed default), and residual bias.

![Figure SI.17. Hex binned spatial distribution of basal area improvement (calibrated RMSE reduction) across CONUS FIA plots.](../calibration/output/comparisons/manuscript_figures/S1_spatial_ba_improvement_hex.png){width=6.5in}

![Figure SI.18. Hex binned calibration win rate across CONUS: share of FIA conditions in each hex where the calibrated prediction outperformed the default.](../calibration/output/comparisons/manuscript_figures/S2_spatial_calib_winrate_hex.png){width=6.5in}

![Figure SI.19. Per variant performance map. Each variant polygon is colored by its calibrated composite RMSE improvement percentage.](../calibration/output/comparisons/manuscript_figures/S3_variant_performance_map.png){width=6.5in}

![Figure SI.20. Climate site index versus calibration improvement: scatter relating the ClimateSI proxy for site productivity to the magnitude of calibration improvement.](../calibration/output/comparisons/manuscript_figures/S4_climatesi_vs_improvement.png){width=6in}

![Figure SI.21. Bivariate site index and improvement map. Two dimensional choropleth showing the joint distribution of site productivity and calibration improvement.](../calibration/output/comparisons/manuscript_figures/S5_bivariate_si_improvement_map.png){width=6.5in}

![Figure SI.22. Scatter of stand density index maximum revision (percent lower than default) versus calibration improvement per variant.](../calibration/output/comparisons/manuscript_figures/S6_sdimax_vs_improvement.png){width=6in}

![Figure SI.23. Spatial distribution of basal area residual bias under default (top) and calibrated (bottom) configurations.](../calibration/output/comparisons/manuscript_figures/S7_spatial_ba_residual_bias.png){width=6.5in}

![Figure SI.24. Hex binned spatial distribution of volume improvement across CONUS.](../calibration/output/comparisons/manuscript_figures/S8_spatial_vol_improvement_hex.png){width=6.5in}

![Figure SI.25. Spatial volume RMSE maps: calibrated versus default panels, side by side.](../calibration/output/comparisons/manuscript_figures/S9_spatial_vol_rmse_calib_vs_default.png){width=6.5in}

![Figure SI.26. Volume win rate across CONUS crossed with ClimateSI proxy, exposing regions where climate site productivity interacts with calibration benefit.](../calibration/output/comparisons/manuscript_figures/S10_vol_winrate_and_climatesi.png){width=6.5in}

![Figure SI.27. Spatial distribution of volume residual bias under calibrated parameters.](../calibration/output/comparisons/manuscript_figures/S11_spatial_vol_residual_bias.png){width=6.5in}

# Part 4. Component level posterior diagnostics

![Figure SI.28. Calibration heatmap: cells colored by per variant, per component improvement percentage.](../calibration/output/comparisons/manuscript_figures/fig_calibration_heatmap.png){width=6in}

![Figure SI.29. Per variant confidence intervals (all components, all variants).](../calibration/output/comparisons/manuscript_figures/fig_ci_all_variants.png){width=6in}

![Figure SI.30. Per region confidence interval comparison, grouping variants geographically (Pacific Coast, Interior West, Rockies, Central, East).](../calibration/output/comparisons/manuscript_figures/fig_ci_by_region.png){width=6in}

![Figure SI.31. Crown ratio model version comparison (v1 versus v2). v2 introduced species random intercepts and heteroskedastic variance; v1 used pooled fixed effects.](../calibration/output/comparisons/manuscript_figures/fig_cr_v1_v2_comparison.png){width=6in}

![Figure SI.32. Mortality model version comparison (v1 versus v2). v2 uses species random intercepts with competition only fixed effects; v1 included all 13 covariates with fixed effects only.](../calibration/output/comparisons/manuscript_figures/fig_mortality_v1_v2_comparison.png){width=6in}

![Figure SI.33. Height to diameter improvement per variant.](../calibration/output/comparisons/manuscript_figures/fig_hd_improvement.png){width=6in}

![Figure SI.34. V3 composite RMSE by variant (most recent calibration generation).](../calibration/output/comparisons/manuscript_figures/v3_composite_pctrmse_by_variant.png){width=6in}

![Figure SI.35. V3 dumbbell per variant: default versus calibrated composite RMSE percent.](../calibration/output/comparisons/manuscript_figures/v3_dumbbell_pctrmse.png){width=6in}

![Figure SI.36. V3 R squared heatmap: per variant, per component R squared values in tabular format.](../calibration/output/comparisons/manuscript_figures/v3_r2_heatmap.png){width=6in}

![Figure SI.37. V3 spatial performance map.](../calibration/output/comparisons/manuscript_figures/v3_spatial_performance_map.png){width=6.5in}

![Figure SI.38. Conceptual diagram showing data flow from FIA remeasurement through Bayesian calibration, posterior JSON export, runtime parameter swapping via fvs2py and rFVS, and uncertainty propagation through stand projections.](../calibration/output/comparisons/manuscript_figures/fig_conceptual_diagram_v2.png){width=6in}

# Part 5. Bakuzis matrix Acadian variant

The main text Figures 8 through 11 show the Northeast (NE) variant
results. The Acadian (ACD) variant was run in the same SLURM array;
its trajectory grid, year 100 divergence, and posterior band growth
panels are reproduced here for completeness. The ACD variant shows
substantially higher Eichhorn compliance under the posterior band
than under either point estimate (50 percent vs 17 percent), which
is the headline finding for this part of the analysis.

![Figure SI.39. Bakuzis matrix 100 year BA trajectories for the Acadian variant.](../calibration/output/comparisons/manuscript_figures/fig_bakuzis_trajectories_acd.png){width=6.5in}

![Figure SI.40. Year 100 BA divergence for the Acadian variant.](../calibration/output/comparisons/manuscript_figures/fig_bakuzis_divergence_acd.png){width=6in}

![Figure SI.41. Posterior uncertainty band growth with horizon for the Acadian variant.](../calibration/output/comparisons/manuscript_figures/fig_bakuzis_band_growth_acd.png){width=6in}

# Note on calibration parameter representation

The calibrated parameters distributed under `config/calibrated/` retain
the original FVS coefficient arrays nearly unchanged. The Bayesian
posterior is applied at runtime through species-specific multiplier
keywords (GROWMULT for diameter growth, MORTMULT for mortality,
SDIMAX for stand density maximum, HTGMULT for height growth) rather
than as direct replacement of the underlying B1, B2, or other
coefficients. This architecture preserves backward compatibility with
existing FVS keyword files and allows users to mix calibrated and
default parameters per component or per species. As a consequence, a
direct coefficient comparison between `config/ne.json` and
`config/calibrated/ne.json` shows mean B1 changes of order 2 to 3
percent across approximately 85 of 108 species (in the Northeast
variant) but the operationally meaningful variation is carried in the
per-species multipliers stored under `config/calibrated/<variant>_draws.json`.
The `FvsConfigLoader.compare()` helper in
`config/config_loader.py` computes both representations and a
summary table can be regenerated with `python3 -c "from
config.config_loader import compare_configs; print(compare_configs('ne'))"`.

# Notes on reproducibility

All raw calibration outputs (posterior draws, convergence diagnostics,
per variant fit summaries) are archived in
calibration/output/variants/&lt;variant&gt;/ under the fvs-modern public
repository. Every figure in this supplement can be regenerated by
running the corresponding R script under calibration/R/ with the FIA
remeasurement data fetched via calibration/R/01_fetch_fia_data.R. The
spatial maps require the FIA plot coordinate database (privacy
protected; coordinates are perturbed) which is obtained separately
from the USDA FS Forest Service FIA DataMart.

For the Bakuzis matrix uncertainty figures (Figures 8 through 11 in
the main text), the ensemble is produced by the Python scripts
under calibration/python/bakuzis_uncertainty_comparison.py and
bakuzis_uncertainty_aggregate.py; figures are rendered through either
the matplotlib script (bakuzis_uncertainty_figures.py) or the R
script (calibration/R/17_bakuzis_uncertainty_figures.R).

# Script manifest

Summary of the R and Python scripts referenced in the main text:

| Script | Purpose |
|--------|---------|
| calibration/R/01_fetch_fia_data.R            | Fetch FIA ENTIRE dataset, build paired observations |
| calibration/R/02_fit_diameter_growth.R       | Bayesian Wykoff diameter growth |
| calibration/R/03_fit_height_diameter.R       | Chapman Richards height diameter |
| calibration/R/03b_fit_height_increment.R     | Height increment for applicable variants |
| calibration/R/04_fit_mortality.R             | Logistic mortality with annualization |
| calibration/R/05_fit_crown_ratio.R           | Linear crown ratio change |
| calibration/R/06_posterior_to_json.R         | Export calibrated and draws JSON |
| calibration/R/07_diagnostics.R               | Convergence and prior vs posterior checks |
| calibration/R/08_fetch_stand_data.R          | Stand level metrics from FIA |
| calibration/R/09_fit_stand_density.R         | SDImax via quantile regression plus Bayesian |
| calibration/R/16_bakuzis_matrix_figure.R     | Point estimate Bakuzis figure (predecessor) |
| calibration/R/17_bakuzis_uncertainty_figures.R | Uncertainty aware Bakuzis figures (this work) |
| calibration/R/project_future_si_delta.R      | Delta method future SI projection |
| calibration/python/bakuzis_100yr_comparison.py | Point estimate 100 year Bakuzis runner |
| calibration/python/bakuzis_uncertainty_comparison.py | Uncertainty aware 100 year Bakuzis runner |
| calibration/python/bakuzis_uncertainty_aggregate.py  | Ensemble aggregation to bands |
| calibration/python/bakuzis_uncertainty_figures.py    | Matplotlib publication figures |
| calibration/python/bakuzis_preview_figures.py        | Preview figures from existing CSV |
| calibration/python/perseus_100yr_projection.py       | Perseus plot level projection (Maine FIA) |
| calibration/python/perseus_uncertainty_projection.py | Perseus with posterior draws |
| calibration/python/perseus_climate_projection.py     | Perseus with climate modified SI |
| calibration/python/perseus_harvest_projection.py     | Perseus harvest factorial |
| calibration/python/perseus_aggregate.py              | Perseus aggregation utilities |
| calibration/slurm/submit_bakuzis_uncertainty.sh      | Bakuzis uncertainty SLURM template |
| calibration/slurm/submit_perseus_climate.sh          | Perseus climate SLURM template |
| calibration/slurm/submit_perseus_harvest.sh          | Perseus harvest factorial SLURM |
