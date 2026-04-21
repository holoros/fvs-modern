# Execution Guide: Hero Figures Script

**Script:** `22_hero_figures.R`
**Status:** Production ready ✓
**Last Tested:** 2026-04-04

## Quick Start

```bash
cd <repo-root>/fvs-modern
Rscript calibration/R/22_hero_figures.R
```

The script will:
1. Load calibration performance data
2. Generate Figure 1 (6-panel "Complete Story")
3. Generate Figure 2 (6-panel "FIA Benchmark")
4. Save as PNG (300 dpi) and PDF
5. Print summary statistics

**Expected Runtime:** ~30-45 seconds

## Output Structure

```
calibration/output/comparisons/manuscript_figures/
├── fig1_complete_story.png      (685 KB) ← Main publication figure
├── fig1_complete_story.pdf      (39 KB)  ← For editorial submission
├── fig2_fia_benchmark.png       (320 KB) ← Validation figure
└── fig2_fia_benchmark.pdf       (7.6 KB)
```

## Figure Specifications

### Figure 1: The Complete Story

**Dimensions:** 18" wide × 14" tall (3 rows × 2 columns)

**Panels:**
| Panel | Component | Purpose | Data Source |
|-------|-----------|---------|-------------|
| A | Heatmap (4×25) | Calibration performance | model_performance_summary.csv |
| B | Heatmap (4×25) | Ecological realism | bakuzis_*.csv files |
| C | Faceted lines | Trajectory comparison | stand_projections_*.csv |
| D | Scatter | Year-50 BA comparison | stand_projections_with_ci.csv |
| E | Line comparison | Annualization diagnostic | stand_projections_3scenarios.csv |

**Key Features:**
- Okabe-Ito colorblind-safe palette
- 81% pass rate on Bakuzis ecological realism tests
- Shows improvement across all 25 variants
- Demonstrates critical annualization fix

### Figure 2: FIA Benchmark

**Dimensions:** 18" wide × 14" tall (3 rows × 2 columns)

**Panels:**
| Panel | Component | Purpose | Status |
|-------|-----------|---------|--------|
| A | Scatter (calib) | Predictive validation | Placeholder ready |
| B | Scatter (default) | Default comparison | Placeholder ready |
| C | Boxplots | Residual distribution | Placeholder ready |
| D | Scatter (volume) | Volume predictions | Placeholder (NSBE pending) |
| E | Bar chart | MAI comparison | Ready (uses stand_3scenarios) |
| F | Bar chart | Bias by variant | Ready (uses stand_level data) |

**Note:** Panels A-D will auto-populate when FIA benchmark results are available at:
```
calibration/output/comparisons/fia_benchmark_results.csv
```

## Dependencies

All dependencies are standard R packages:
- `tidyverse` - Data manipulation
- `ggplot2` - Visualization
- `patchwork` - Multi-panel assembly
- `scales` - Utility functions

## Customization

### Change Output Dimensions

Edit lines ~275 and ~675:

```r
# Current (18 × 14):
ggsave(..., width = 18, height = 14, ...)

# For smaller figures (e.g., 12 × 10):
ggsave(..., width = 12, height = 10, ...)
```

### Change DPI

Edit the `dpi` parameter:

```r
# Current (300 dpi):
ggsave(..., dpi = 300, ...)

# For draft quality (72 dpi):
ggsave(..., dpi = 72, ...)
```

### Change Color Palette

Edit the `region_colors` vector (~line 65):

```r
region_colors <- c(
  "Pacific Northwest" = "#YOUR_COLOR_HEX",
  # ... etc
)
```

## Performance Metrics (Excerpt from Last Run)

```
Bakuzis Matrix Pass Rates:
  - Sukachev Effect (competition): 100% (25/25)
  - Eichhorn's Rule (site effect): 84% (21/25)
  - Crown Recession: 100% (25/25)
  - Mortality U-Shape: 40% (10/25)

Overall: 81% (81/100 tests passing)

Height-Diameter Improvement: +0.030 (calibrated vs default)
```

## Troubleshooting

### Error: "Can't select columns that don't exist"

**Cause:** Data file column names changed
**Solution:** Check column names in source CSV files:

```bash
head -1 calibration/output/comparisons/model_performance_summary.csv
head -1 calibration/output/assessment/bakuzis_*.csv
```

### Error: "object 'region' not found"

**Cause:** Data join issue with variant_info
**Solution:** Verify variant_info mapping includes all 25 variants:

```r
# In script ~line 60:
variant_info <- tibble(
  variant = c("AK", "BC", ..., "ACD"),  # Should have 25
  region = c(...)                        # Should have 25
)
```

### Figures are blank or mostly white

**Cause:** Data loading failed silently
**Solution:** Check that data files exist:

```bash
ls -lh calibration/output/comparisons/stand_projections_*.csv
ls -lh calibration/output/assessment/bakuzis_*.csv
```

## Integration Points

### For FIA Benchmark (Figure 2, Panels A-B, C)

When FIA benchmark results are computed, place them at:
```
calibration/output/comparisons/fia_benchmark_results.csv
```

Expected columns:
```
variant, condition_id, ba_pred_calib, ba_obs, ba_pred_default,
rmse_calib, rmse_default
```

The script will auto-detect and populate panels A-C.

### For Volume Integration (Figure 2, Panel D)

Once NSBE (National Species-Based Equations) equations are integrated, add volume columns to the stand projection data. Panel D structure is ready to display:
```
vol_pred_calib vs vol_obs with 1:1 line and RMSE
```

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2026-04-04 | 1.0 | Initial production release |
| | | - 6-panel Figure 1: Complete Story |
| | | - 6-panel Figure 2: FIA Benchmark (placeholder ready) |
| | | - Okabe-Ito colorblind palette |
| | | - 300 dpi PNG + PDF output |

## Contact & Support

For questions or issues:
1. Check this execution guide
2. Review the detailed README (`22_HERO_FIGURES_README.md`)
3. Inspect the script comments (~line 5-30 for overview)

## Notes

- Script is fully self-contained and uses only FVS_PROJECT_ROOT variable
- All figure paths are automatically created
- No user input required (fully automated)
- Safe to run multiple times (overwrites previous output)
